-- $Id: random_tools.adb,v 1.1 2008/11/19 12:04:57 baker Exp $

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;

package body Random_Tools is

  Bits: constant:=24;
  type Int is range -2**Bits..2**Bits-1;
  package Int_IO is new Integer_IO(Int); use Int_IO;
  package Flt_IO is new Float_io(Float); use Flt_IO;

  M3: constant := 97;
  Init_C: constant:= 362436;
  CD:  constant:= 7654321;
  CM:  constant:= (2**Bits)-3;

  subtype Range_1 is integer range 0..M1-1;
  subtype Range_2 is integer range 0..M2-1;
  subtype Range_3 is INTEGER range 1 .. M3;

  type U_Array is array (Range_3) of Int;
  type State is
       record U : U_Array;
              C : Int;
       end record;

  States : File_Type;
  Mode: Boolean:= false;
  I, J, K: Range_1;
  L: Range_2;
  NI, NJ: integer;
  C: Int;
  U: U_Array;

--
-- This procedure initializes the table for F(M3,Range_3'last/3,-MOD 1.0)
-- generator and produces values for the arithmetic sequence.
--
procedure Start(New_I, New_J, New_K: Seed_Range_1; New_L: Seed_Range_2;
                Antithetic: Boolean) is
  S: Int;
  M: Range_1;
begin
  Mode:= Antithetic;
  I:=New_I; J:=New_J; K:=New_K; L:=New_L;
  NI:= Range_3'last; NJ:= Range_3'last/3+1;
  C:=Init_C;
  for II in Range_3 loop
    S:= 0;
    for JJ in 1..Bits loop
      M:= (((J*K) mod M1)*I) mod M1;
      I:= J; J:= K; K:= M;
      L:= (53*L+1) mod M2;
      S:=S*2;
      if (((L*M) mod 64) >= 32) then S:= S + 1; end if;
    end loop;
    U(II):= S;
  end loop;
end Start;


procedure Backup_Random_State(Data_File: string) is
  N1,N2 : Int;
begin
  N1 := Int(NI);   N2 := Int(NJ);
  create(States,out_file,Data_File);
  for I in Range_3 loop
    if I mod 5 = 0 then new_line(States); end if;
    put(States,U(I));
  end loop;
  put(States,C); put(States,N1); put(States,N2);
  close(States);
end Backup_Random_State;


procedure Restore_Random_State(Data_File : string) is
  N1,N2 : Int;
begin
  open(States,in_file,Data_File);
  if not end_of_file(States) then
    for I in Range_3 loop
      if end_of_line(States) then skip_line(States); end if;
      get(States,U(I));
    end loop;
    get(States,C); get(States,N1); get(States,N2);
    NI := integer(N1);  NJ := integer(N2);
  end if;
  close(States);
end Restore_Random_State;


function Random return FLOAT is
  Temp: Int;
begin
  Temp:= U(NI)-U(NJ);
  if (Temp < 0) then Temp:= Temp+1+Int'last; end if;
  U(NI):= Temp;
  NI:= NI-1;
  if (NI=0) then NI:=Range_3'last; end if;
  NJ:= NJ-1;
  if (NJ=0) then NJ:=Range_3'last; end if;
  C:= C-CD;
  if (C < 0) then C:=C+CM; end if;
  Temp:= Temp-C;
  if (Temp < 0) then Temp:= Temp+1+Int'last; end if;
  if Mode then
    return 1.0-FLOAT(Temp)*(2.0**(-Bits));
  else
    return FLOAT(Temp)*(2.0**(-Bits));
  end if;
end Random;


function Exp_Dist(m: Float) return integer is
-- should return a random value in the range 1 .. integer'last
-- with approximately the inverse exponential distribution:
--                        A(x)=1-e**(-x/m).
-- The number m is the mean value of this distribution.
  t: Float;
  i: integer;
  use Ada.Numerics.Elementary_Functions;
begin
   t:=random;
   pragma Assert (t>=0.0 and t<=1.0, "Exp_Dist (bad random number)");
   if t=1.0 then t:=0.0; end if;
   -- 0.0 <= t < 1.0
   t:= -log(t) * m;
   -- 0.0 <= t < infty
   if t>=Float(integer'last) then return integer'last;
   end if;
   -- 0.0 <= t <= integer'last
   i:= integer(t-0.5);
   -- 0 <= t <= integer'last
   -- ??? what is the story on the following code?
   --  if float(i)<t then i:=i+1;
   --  elsif i=0 then i:=1;
   --  end if;
   return i;
end Exp_Dist;

function Poisson(m: in Float) return integer is
  p, f, u: Float;
  x: integer;
  use Ada.Numerics.Elementary_Functions;
begin
  p:= exp(-m);
  f:= p;
  x:= 0;
  u:= Random;
  while u>f loop
    x:= x + 1;
     p:= (m/Float(x))*p;
     f:= f + p;
  end loop;
  return x;
end Poisson;

end Random_Tools;
