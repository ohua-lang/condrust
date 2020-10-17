module Ohua.Integration.Lang where

data Lang = Rust

data Language :: Lang -> * where
    SRust :: Language 'Rust
