mod token_type_ext;
mod ast_node;

use synstructure::decl_derive;

decl_derive! {
    [TokenKindExt, attributes(symbol)] => token_type_ext::derive_token_kind_ext
}

decl_derive! {
    [AstNode] => ast_node::derive_ast_node
}

// pub fn derive_ast_node(input: TokenStream) -> TokenStream {
//
//     todo!()
// }
