module type Semigroup_base = sig
  type t

  val sum: t -> t -> t
end

module type Semigroup = sig
  include Semigroup_base
  val (|+|): t -> t -> t
end

module Semigroup (S: Semigroup_base): Semigroup with type t = S.t = struct
  include S

  let (|+|) t1 t2 = sum t1 t2
end

module type Monoid_base = sig
  type t
  include Semigroup_base with type t := t

  val zero: t
end

module type Monoid = sig
  include Monoid_base
  include Semigroup with type t := t
end

module Monoid (M: Monoid_base): Monoid with type t = M.t = struct
  include M
  module S: Semigroup with type t := M.t = Semigroup(M)
  include S
end
