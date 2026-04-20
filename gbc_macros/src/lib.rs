mod token_type_ext;
use synstructure::decl_derive;

decl_derive! {
    [TokenTypeExt, attributes(symbol)] => token_type_ext::derive_token_type_ext
}

// pub fn derive_ast_node(input: TokenStream) -> TokenStream {
//
//     todo!()
// }
