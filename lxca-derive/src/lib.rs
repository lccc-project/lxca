use proc_macro::{Punct, TokenStream};

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned};
use syn::{
    Attribute, Data, DeriveInput, Fields, Ident, Lifetime, LitStr, Meta, Path, PathSegment,
    PredicateType, Token, TraitBound, TypeParamBound, WherePredicate, parse_quote,
    parse_quote_spanned, punctuated::Punctuated, spanned::Spanned, token::PathSep,
};

#[proc_macro_derive(DebugWithConstants, attributes(debug_with_constants))]
pub fn derive_debug(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    derive_debug_inner(input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn debug_arm_from_name_and_fields(
    prefix: Option<Ident>,
    name: Ident,
    fields: Fields,
    fmt_name: Ident,
    constants_name: Ident,
    with_constants: Path,
    span: Span,
    finish_name: Ident,
) -> TokenStream2 {
    let mut path: Punctuated<Ident, Token![::]> = Punctuated::new();
    if let Some(prefix) = prefix {
        path.push_value(prefix);
    }
    let name_str = LitStr::new(&name.to_string(), name.span());
    path.push(name);

    match fields {
        Fields::Unit => {
            quote! {#path => {
                #fmt_name . write_str(#name_str)
            }}
        }
        Fields::Unnamed(un) => {
            let names = un
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| Ident::new_raw(&format!("__{i}"), span));

            let fields: Punctuated<Ident, Token![,]> = names.clone().collect();

            let calls = names
                .map(|v| quote!(.field(&#with_constants (& #v, #constants_name))))
                .collect::<TokenStream2>();

            quote! {
                #path (#fields) => {
                    #fmt_name . debug_tuple (#name_str)
                        #calls
                        . #finish_name ()

                }
            }
        }
        Fields::Named(n) => {
            let names = n.named.iter().map(|f| f.ident.clone().unwrap());

            let fields: Punctuated<Ident, Token![,]> = names.clone().collect();

            let calls = names
                .map(|v| (LitStr::new(&v.to_string(), v.span()), v))
                .map(
                    |(name, field)| quote!(.field(#name, &#with_constants (#field, #constants_name))),
                )
                .collect::<TokenStream2>();

            quote! {
                #path {#fields} => {
                    #fmt_name . debug_struct (#name_str)
                        #calls
                        . #finish_name ()

                }
            }
        }
    }
}

struct Attrs {
    finish_name: Ident,
    crate_name: Ident,
    lifetime_name: Lifetime,
}

impl Default for Attrs {
    fn default() -> Self {
        Self {
            finish_name: Ident::new_raw("finish", Span::mixed_site()),
            crate_name: Ident::new_raw("lxca", Span::mixed_site()),
            lifetime_name: Lifetime::new("'ir", Span::mixed_site()),
        }
    }
}

impl<'a> Extend<&'a Attribute> for Attrs {
    fn extend<T: IntoIterator<Item = &'a Attribute>>(&mut self, iter: T) {
        for attr in iter {
            match &attr.meta {
                Meta::Path(p) if p.is_ident("non_exhaustive") => {
                    self.finish_name = Ident::new_raw("finish_non_exhaustive", p.span())
                }
                _ => {}
            }
        }
    }
}

impl<'a> FromIterator<&'a Attribute> for Attrs {
    fn from_iter<T: IntoIterator<Item = &'a Attribute>>(iter: T) -> Self {
        let mut val = Attrs::default();

        val.extend(iter);

        val
    }
}

fn derive_debug_inner(mut input: DeriveInput) -> Result<TokenStream2, syn::Error> {
    let span = proc_macro2::Span::mixed_site();

    let mut clause = input.generics.make_where_clause();

    let Attrs {
        crate_name,
        finish_name,
        lifetime_name,
    } = input.attrs.iter().collect();

    let mut attrs = Vec::<Attribute>::new();
    attrs.push(parse_quote_spanned! {span => #[automatically_derived]});

    let fmt_name = Ident::new_raw("__fmt", Span::mixed_site());
    let consts_name = Ident::new_raw("__consts", Span::mixed_site());

    let path_span = Span::mixed_site();

    let trait_name: Path =
        parse_quote!( :: #crate_name :: fmt_helpers :: DebugWithConstants :: <#lifetime_name> );

    let with_constants: Path = parse_quote!(:: #crate_name :: fmt_helpers :: WithConstants);

    let arms: Punctuated<TokenStream2, Token![,]> = match input.data {
        Data::Struct(n) => {
            for n in n.fields.iter() {
                clause.predicates.push(WherePredicate::Type(PredicateType {
                    lifetimes: None,
                    bounded_ty: n.ty.clone(),
                    colon_token: Token![:](n.span()),
                    bounds: [TypeParamBound::Trait(TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: trait_name.clone(),
                    })]
                    .into_iter()
                    .collect(),
                }))
            }

            core::iter::once(debug_arm_from_name_and_fields(
                None,
                input.ident.clone(),
                n.fields,
                fmt_name.clone(),
                consts_name.clone(),
                with_constants,
                span,
                finish_name,
            ))
            .collect()
        }
        Data::Enum(e) => e
            .variants
            .into_iter()
            .map(|v| {
                debug_arm_from_name_and_fields(
                    Some(input.ident.clone()),
                    v.ident,
                    v.fields,
                    fmt_name.clone(),
                    consts_name.clone(),
                    with_constants.clone(),
                    span,
                    finish_name.clone(),
                )
            })
            .collect(),
        Data::Union(u) => Err(syn::Error::new_spanned(
            u.union_token,
            "`derive(DebugWithConstants)` is not supported for unions",
        ))?,
    };

    let (impl_gen, ty_gen, where_clauses) = input.generics.split_for_impl();

    let ident = input.ident;

    Ok(quote! {
        impl #impl_gen #trait_name for #ident #ty_gen #where_clauses {
            fn fmt(&self, #fmt_name: &mut core::fmt::Formatter, #consts_name: &:: #crate_name :: ir :: constant :: ConstantPool<#lifetime_name>) -> core::fmt::Result {
                match self {
                    #arms
                }
            }
        }
    })
}
