-- $Id: cycles.adb,v 1.1 2008/11/23 21:33:20 baker Exp $

package body Cycles is

function GCD (A, B : in Positive) return positive is
   X : Integer := A;
   Y : Integer := B;
   T : Integer;
begin
   while X /= 0 loop
      T := Y mod X; Y := X; X := T;
   end loop;
   return Y;
end GCD;


function LCM (A, B : in positive) return positive is
-- LCM (A, B) =  (A * B) / GCD (A, B)
begin
   return  (A / GCD (A, B)) * B;
end LCM;


function GCD (P : Positive_Vector) return positive is
   X : Integer :=  P (P'last);
begin
   for I in P'first .. P'last - 1 loop
     X := GCD (P (I), X);
   end loop;
   return X;
end GCD;


function LCM (P : Positive_Vector) return positive is
   X : Integer :=  P (P'last);
begin
   for I in P'first .. P'last - 1 loop
     X := LCM (P (I), X);
   end loop;
   return X;
end LCM;


function Min (P : Int_Vector) return integer is
   X : Integer := P (P'last);
begin
   for I in P'first .. P'last - 1 loop
      if P (I) < X then
         X := P (I);
      end if;
   end loop;
   return X;
end Min;


function Max (P : Int_Vector) return integer is
   X : Integer := P (P'last);
begin
   for I in P'first .. P'last - 1 loop
      if P (I) > x then
         X := P (I);
      end if;
   end loop;
   return X;
end Max;


function Round_To_A_Factor (M, F : integer) return integer is
   R : integer := M mod F; -- remainder;
   D : integer := M  /  F; -- divisor;
   N : integer := F;
   L : integer;
begin
   if D = 0 then return 0; end if;
   if R = 0 then return N; end if;
   if R mod D = 0 then return N + R / D; end if;
   L := 1;
   loop
      if D - L = 0 then
         return M;
      end if;
      if  (N * L + R) mod  (D - L) = 0 then
         N := N +  (N * L + R) /  (D - L);
         exit;
      end if;
      L := L + 1;
   end loop;
   return N;
end Round_To_A_Factor;


function Ceiling (x : float) return integer is
   I : Integer;
begin
   I := Integer (X);
   if X > 0.0 and then Float (I) < X then
      return I + 1;
   end if;
   return I;
end Ceiling;

function Ceiling_Div (A, B : Integer) return Integer is
--  ceiling (A / B)
  Q : Integer := A / B;
begin
   if A <= 0 then
      return 1;
   elsif Q * B < A then
      return Q + 1;
   else
      return Q;
   end if;
end Ceiling_Div;

end Cycles;
