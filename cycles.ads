-- $Id: cycles.ads,v 1.1 2008/11/23 21:33:20 baker Exp $

package Cycles is
   
  type Positive_Vector is array  (Positive range <>) of Positive;
  type Int_Vector is array (Integer range <>) of Integer;

  function GCD (A,B : in Positive) return Positive;
  function LCM (A,B : in Positive) return Positive;
  function GCD (P : Positive_Vector) return Positive;
  function LCM (P : Positive_Vector) return Positive;
  function Min (P : Int_Vector) return Integer;
  function Max (P : Int_Vector) return Integer;
  function Round_To_A_Factor (M,F : Integer) return Integer;
  function Ceiling (X : Float) return Integer;
  function Ceiling_Div (A, B : Integer) return Integer;

end Cycles;
