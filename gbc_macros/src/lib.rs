mod token_type_ext;
use synstructure::decl_derive;

decl_derive! {
    [TokenKindExt, attributes(symbol)] => token_type_ext::derive_token_kind_ext
}

// pub fn derive_ast_node(input: TokenStream) -> TokenStream {
//
//     todo!()
// }
