(* Library for Numerical Calculation *)

module Mat = Matrix

open Matrix_op

module LESolver = struct
  include LinearEq
  include Type
end

module Diff = struct
  include Differentiation
  include Eval
  include Type
end

module Integral = struct
  include Integral
  include Eval
  include Type
end

module Taylor = struct
  include Taylor
  include Type
end

module NLESolver = struct
  include NonLinearEq
  include Type
end
