--  file : Random_Tools.ads
--  $Id: random_tools.ads,v 1.1 2008/11/19 12:04:57 baker Exp $
--  author : T. Baker, Florida State University
--  purpose : generation of random numbers and some frequently used distributions

--  ??? Is this still good enough that we want to
--  use our own pseudo-random number generation mechanism, instead of the
--  standard Ada one?

package Random_Tools is

  M1: constant:= 179;
  M2: constant:= M1 - 10;

  subtype Seed_Range_1 is integer range 1 .. M1 - 1;
  subtype Seed_Range_2 is integer range 1 .. M2 - 1;

  procedure Start (New_I, New_J, New_K : Seed_Range_1; New_L : Seed_Range_2;
                   Antithetic : Boolean);
  procedure Backup_Random_State (Data_File: String);
  procedure Restore_Random_State (Data_File: String);

  --  get the next pseudo-random value
  function Random return Float;

  --  Exponential distribution with mean value m
  function Exp_Dist (M : in Float) return Integer;

  --  Poisson distribution with mean value m
  function Poisson (M : in Float) return Integer;

end Random_Tools;
