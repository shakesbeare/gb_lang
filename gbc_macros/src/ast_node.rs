use proc_macro2::TokenStream;
use synstructure::Structure;
use quote::quote;


pub fn derive_ast_node(s: Structure) -> TokenStream {
    s.gen_impl(quote! {
        gen impl ::gbc_parse::parse_tree::AstNode for @Self {
            fn span(&self) -> &Span {
                &self.span
            }
        }
    })
}
