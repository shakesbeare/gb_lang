use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{DeriveInput, Ident, parse_macro_input};

extern crate proc_macro;

#[proc_macro_derive(TokenKindExt, attributes(symbol))]
pub fn derive_token_kind_ext(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = match input.data {
        syn::Data::Struct(data_struct) => {
            let field = data_struct.fields.iter().find(|f| match f.ty {
                syn::Type::Path(ref type_path) => type_path.path.is_ident("TokenKind"),
                _ => false,
            });
            let Some(field) = field else {
                panic!("Requires field with type `gbc_lex::TokenKind`");
            };

            let field_name = &field.ident;

            quote! {
                impl #impl_generics crate::TokenKindExt for #name #ty_generics #where_clause {
                    fn is_symbol(&self) -> bool {
                        self.#field_name.is_symbol()
                    }

                    fn is_single_length(&self) -> bool {
                        self.#field_name.is_single_length()
                    }

                    fn is_double_length(&self) ->  bool {
                        self.#field_name.is_double_length()
                    }
                }
            }
        }
        syn::Data::Enum(data_enum) => {
            let singles: Vec<&Ident> = data_enum
                .variants
                .iter()
                .filter(|v| {
                    let Some(symbol_attr) = v.attrs.iter().find(|a| a.path().is_ident("symbol"))
                    else {
                        return false;
                    };
                    let arg: Ident = symbol_attr.parse_args().unwrap();
                    arg == "single"
                }).map(|v| &v.ident)
                .collect();

            let doubles: Vec<&Ident> = data_enum
                .variants
                .iter()
                .filter(|v| {
                    let Some(symbol_attr) = v.attrs.iter().find(|a| a.path().is_ident("symbol"))
                    else {
                        return false;
                    };
                    let arg: Ident = symbol_attr.parse_args().unwrap();
                    arg == "double"
                }).map(|v| &v.ident)
                .collect();


            quote! {
                impl #impl_generics crate::TokenKindExt for #name #ty_generics #where_clause {
                    fn is_symbol(&self) -> bool {
                        match self {
                            #(Self::#singles => true, )*
                            #(Self::#doubles  => true, )*
                            _ => false,
                        }
                    }

                    fn is_single_length(&self) -> bool {
                        match self {
                            #(Self::#singles => true, )*
                            _ => false
                        }
                    }

                    fn is_double_length(&self) -> bool {
                        match self {
                            #(Self::#doubles => true, )*
                            _ => false
                        }
                    }
                }
            }
        }
        syn::Data::Union(_) => panic!("Unsupported on unions"),
    };

    proc_macro::TokenStream::from(expanded)
}
