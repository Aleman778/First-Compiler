//! Procedural macro for generating symbols based on given
//! symbols and keyword definition.


use std::collections::HashSet;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, braced, LitStr, Ident, Token};


/**
 * Define custom keywords for `Keywords` and `Symbols`
 */
#[allow(non_camel_case_types)]
mod kw {
    syn::custom_keyword!(Keywords);
    syn::custom_keyword!(Symbols);
}

/**
 * Symbol is defined by an identifier in the case where
 * the identifier is a Rust keyword then symbol can be
 * defined as string e.g. `Let: "let"` (ok) vs. `let` 
 * (error since `let` is a keyword). Then `Let` is used in code
 * but its symbol is defined by the string literal "let" instead.
 */
struct Symbol {
    name: Ident,
    value: Option<LitStr>
}


impl Parse for Symbol {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let value = match input.parse::<Token![:]>() {
            Ok(_) => Some(input.parse()?),
            Err(_) => None,
        };
        input.parse::<Token![,]>()?;
        Ok(Symbol { name, value })
    }
}


/**
 * Helper structure for defining parser on list of symbols.
 */
struct SymbolList(Vec<Symbol>);


impl Parse for SymbolList {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut list = Vec::new();
        while let Ok(sym) = input.parse() {
            list.push(sym);
            
        }
        Ok(SymbolList(list))
    }
}


/**
 * Input data defines list of keywords and symbols.
 */
struct Input {
    keywords: SymbolList,
    symbols: SymbolList,
}


impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<kw::Keywords>()?;
        let content;
        braced!(content in input);
        let keywords = content.parse()?;

        input.parse::<kw::Symbols>()?;
        let content;
        braced!(content in input);
        let symbols = content.parse()?;

        Ok(Input { keywords, symbols })
    }
}


/**
 * Generates symbols based on input keywords and symbols.
 */
pub fn define_symbols(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    
    let mut keywords_stream = quote! {};
    let mut symbols_stream = quote! {};
    let mut prefill_stream = quote! {};
    let mut keys = HashSet::<String>::new();
    let mut counter = 0usize;

    let mut check_duplicate = |s: &str| {
        if !keys.insert(s.to_string()) {
            panic!("symbol of name `{}` is duplicated", s);
        }
    };

    for keyword in &input.keywords.0 {
        let name = &keyword.name;
        let value = match &keyword.value {
            Some(value) => value.value(),
            None => name.to_string(),
        };

        check_duplicate(&value);
 
        keywords_stream.extend(quote! {
            pub const #name: Symbol = Symbol::new(#counter);
        });

        prefill_stream.extend(quote! {
            #value, 
        });

        counter += 1;
    }

    for symbol in &input.symbols.0 {
        let name = &symbol.name;
        let value = match &symbol.value {
            Some(value) => value.value(),
            None => name.to_string(),
        };

        check_duplicate(&value);
 
        symbols_stream.extend(quote! {
            pub const #name: Symbol = Symbol::new(#counter);
        });

        prefill_stream.extend(quote! {
            #value, 
        });

        counter += 1;
    }

    let output = TokenStream::from(quote!{
        #[allow(non_upper_case_globals)]
        pub mod kw {
            use super::Symbol;

            #keywords_stream
        }
        
        #[allow(non_upper_case_globals)]
        pub mod sym {
            use super::Symbol;

            #symbols_stream
        }

        impl<'a> SymbolMap<'a> {
            pub fn new() -> Self {
                Self::prefill(&[
                    #prefill_stream
                ])
            }
        }
    });
    output
}
