# mpdb

`mpdb` is a tool to assist debugging C++ "metaprograms";
that is, programs that are evaluated at compile time.

The long-term goal is to allow debugging things like:

- [ ] preprocessor expansions
- [ ] overload resolution
- [ ] template instantiations
- [ ] `constexpr` evaluations
- ...and more

## Warning

As will probably become _very_ quickly apparent,
this library is a massive work-in-progress,
and should be treated as such.
Do not expect any features to work correctly, 
let alone exist, at this earl stage!
