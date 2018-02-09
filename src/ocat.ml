include Functor
include Fix
include Monoid
include Monad
include Comonad

module Ocat_modules = Ocat_modules

module Experimental = struct
  include Free
  include Cofree

  module Ocat_transformers = Ocat_transformers
end
