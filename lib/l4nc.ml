(* Library for Numerical Calculation *)

module Mat = Matrix

open Matrix_op

module LESolver = LinearEq

open Type

module Diff = struct
  include Differentiation
  include Eval
end

module Integral = struct
  include Integral
  include Eval
end

module Taylor = Taylor

module NLESolver = NonLinearEq

