-- Type declarations for Celestial
-- Author    : David Haley
-- Created   : 26/03/2020
-- Last Edit : 28/01/2023

with Ada.Numerics.Generic_Elementary_Functions;

package body Celestial is

   To_Degrees : constant := Degrees (Full_Circle / 2) / Ada.Numerics.Pi;
   To_Radians : constant := Ada.Numerics.Pi / Radians (Full_Circle / 2);

   package Celestial_Elementry is new
     Ada.Numerics.Generic_Elementary_Functions (Celestial_Real);

   function Sin (X : in Degrees) return Celestial_Real is
      (Celestial_Elementry.Sin (Celestial_Real (X * To_Radians)));

   function Cos (X : in Degrees) return Celestial_Real is
      (Celestial_Elementry.Cos (Celestial_Real (X * To_Radians)));

   function Tan (X : in Degrees) return Celestial_Real is
      (Celestial_Elementry.Tan (Celestial_Real (X * To_Radians)));

   function Arcsin (X : in Celestial_Real) return Degrees is
      (Degrees (Celestial_Elementry.Arcsin (X)) * To_Degrees);

   function Arccos (X : in Celestial_Real) return Degrees is
      (Degrees (Celestial_Elementry.Arccos (X)) * To_Degrees);

   function Arctan (Y : in Celestial_Real;
                    X : in Celestial_Real := 1.0) return Degrees is
      (Degrees (Celestial_Elementry.Arctan (Y, X)) * To_Degrees);

   function Sin (X : in Radians) return Celestial_Real is
      (Celestial_Elementry.Sin (Celestial_Real (X)));

   function Cos (X : in Radians) return Celestial_Real is
      (Celestial_Elementry.Cos (Celestial_Real (X)));

   function Tan (X : in Radians) return Celestial_Real is
      (Celestial_Elementry.Tan (Celestial_Real (X)));

   function Arcsin (X : in Celestial_Real) return Radians is
      (Radians (Celestial_Elementry.Arcsin (X)));

   function Arccos (X : in Celestial_Real) return Radians is
      (Radians (Celestial_Elementry.Arccos (X)));

   function Arctan (Y : in Celestial_Real;
                    X : in Celestial_Real := 1.0) return Radians is
      (Radians (Celestial_Elementry.Arctan (Y, X)));

end Celestial;
