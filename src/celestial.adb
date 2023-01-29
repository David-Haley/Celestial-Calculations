-- Type declarations for Celestial
-- Author    : David Haley
-- Created   : 26/03/2020
-- Last Edit : 26/03/2020

with Ada.Numerics.Generic_Elementary_Functions;

package body Celestial is

   package Celestial_Elementry is new
     Ada.Numerics.Generic_Elementary_Functions (Celestial_Real);

   function sin (X : in Radians) return Celestial_Real is

   begin -- sin
      return Celestial_Elementry.sin (Celestial_Real (X));
   end sin;

   function cos (X : in Radians) return Celestial_Real is

   begin -- cos
      return Celestial_Elementry.cos (Celestial_Real (X));
   end cos;

   function tan (X : in Radians) return Celestial_Real is

   begin -- tan
      return Celestial_Elementry.tan (Celestial_Real (X));
   end tan;

   function arcsin (X : in Celestial_Real) return Radians is

   begin -- arcsin
      return Radians (Celestial_Elementry.arcsin (X));
   end arcsin;

   function arccos (X : in Celestial_Real) return Radians is

   begin -- arccos
      return Radians (Celestial_Elementry.arccos (X));
   end arccos;

   function arctan (Y : in Celestial_Real; X : in Celestial_Real := 1.0)
                    return Radians is

   begin -- arctan
      return Radians (Celestial_Elementry.arctan (Y, X));
   end arctan;

end Celestial;
