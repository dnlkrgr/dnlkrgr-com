---
title: GHC-API for dummies
---

The GHC API allows you to use parts of GHC's functionality from inside your Haskell code.

This post is not intended to be taken as accurate documentation! The goal of this post is to give you an overview of the data types most interesting for you to start using the abstract syntax tree (AST) of Haskell files. 

If you compare the explanations here with the official Haddock on hackage you will probably first realize that I omitted the pass and extension (you may wanna check out the [Trees That Grow](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow/trees-that-grow-guidance) paper) parameters. I also omitted other type parameters that I think aren't important for getting an overview over the API. Also, I will sometimes display record types as product types because I believe it's more readable for a post like this.

The first module we will look into is HsSyn (short for Haskell Syntax).

# `module HsSyn`
The data type for a haskell module. 


```haskell
data HsModule = HsModule { 
    hsmodName    :: Maybe (Located ModuleName)
  , hsmodExports :: Maybe (Located [LIE])
  , hsmodImports :: [LImportDecl]
  , hsmodDecls   :: [LHsDecl]
  }

type LHsDecl = Located HsDecl
```

All AST elements are encapsulated by the `Located` type and have type synonyms (prefixed with 'L') for that.
You might think that is annoying but it's actually really helpful, because it is used for annotation of the AST elements with their position in the source code.

Next, let's look at the part where most of the action happens: the declarations.

# `module HsDecl`

There are many types of declarations in Haskell.
Here are in my opinion the four most important ones:

```haskell
data HsDecl = 
      TyClD TyClDecl
    | InstD InstDecl
    | ValD  HsBind
    | SigD  Sig
    | ...
```
* `TyClD TyClDecl`
  - type classes
  - type families
  - data families
  - pattern synonyms
  - data declarations
* `InstD InstDecl`
  - class instances
  - type family instances
  - data family instances
* `ValD HsBind`
  - functions declarations
* `SigD Sig`
  - function type signatures
  - pattern synonym signatures
  - fixity declaration (`infixl 8 ***`)
  - pragmas: INLINE, SPECIALISE, MINIMAL, SCC, COMPLETE

```haskell
data TyClDecl = 
      FamDecl FamilDecl
    | SynDecl (Located IdP) LHsQTyVars LHstype
    | DataDecl (Located IdP) LHsQTyVars HsDataDefn
```

`HsBind` and `Sig` are declared in the HsBinds module.

# `module HsBinds`

TODO: better intro
These include type signatures for functions and pattern synonyms.

TODO: handle FunBind

A number of functions / patterns can have the same type and can be specified in one type signature, that's why we have a list of identifiers here.

```haskell
data Sig = 
    TypeSig   [Located (IdP)] LHsSigWcType
  | PatSynSig [Located (IdP)] LHsSigWcType
  | ...

type LHsSigWcType = HsWildCardBndrs LHsSigType
type LHsSigType   = HsImplicitBndrs LHsType
```

`LHsSigWcType` is where the actual type lives.
It is a wrapper around `HsType`, the AST element for Haskell types.

# `module HsType`

Here some of the representations for Haskell types.
Can you guess how the unit / () type looks like?

```haskell
data HsType =
      HsForAllty LHsType
    | HsTyvar    (Located IdP)
    | HsListTy   LHsType
    | HsTupleTy  [LHsType]
    | HsSumTy    [LHsType]
    | HsQualTy   LHsContext LHsType
    | HsFunTy    LHsType    LHsType
    | ...

type LHsContext = Located HsContext
type HsContext  = [LHsType]
```

TODO: HsExpr