
/***************************************************************************
 * Item declarations are e.g. functions.
 ***************************************************************************/



#[derive(Debug)]
pub struct Item<'ast> {
    /// Kind of item.
    pub kind: ItemKind<'ast>,
    /// Location of item.
    pub span: Span,
}


#[derive(Debug)]
pub enum ItemKind<'ast> {
    Fn(FnSig<'ast>, NodeId)
}


#[derive(Debug)]
pub struct FnSig<'ast> {
    
}


#[derive(Debug)]
pub struct Body {
    
}
