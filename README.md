# ocat

This library provides support for programming with common abstractions from
category theory, including [Functors](src/functor.ml), [Monads](src/monad.ml)
and [Bananas](src/fix.ml). There are some implementations for the 
[core OCaml types](src/ocat_modules.ml) as well.


## Usage

Until I manage to write up API documentation, a tutorial and all the other
helpful things that are currently missing, there are some pointers in 
[the examples](examples/README.md) that should get you started.


## Credits

Many thanks go to

* [Bartosz Milewski](https://bartoszmilewski.com), for sparking my interest
  in category theory
* [Cats](https://typelevel.org/cats/ "The library!"), where I got most of my inspiration for
  writing this library
* [Adventures in Uncertainty](http://blog.sumtypeofway.com/), for an awesome
  introduction to fix-point recursion

## License

GPLv3, see [here](LICENSE).

