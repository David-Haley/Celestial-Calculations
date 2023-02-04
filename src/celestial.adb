-- Type declarations for Celestial
-- Author    : David Haley
-- Created   : 26/03/2020
-- Last Edit : 03/02/2023

-- 20230203 : Pi and Other functions section added. To_Degrees correvtedd to
-- match Degrees range
-- 20230131 : Decimal_Hours, Julian_Days, Radians and Degreed, made types rather
-- than sub types to render them incompatible, explicit type conversions
-- required in calculations. Degree trig functions added;

with Ada.Numerics.Generic_Elementary_Functions;

package body Celestial is

   package Celestial_Elementry is new
     Ada.Numerics.Generic_Elementary_Functions (Celestial_Real);

   function Sin (X : in Degrees) return Celestial_Real is
      (Celestial_Elementry.Sin (Celestial_Real (To_Radians (X))));

   function Cos (X : in Degrees) return Celestial_Real is
      (Celestial_Elementry.Cos (Celestial_Real (To_Radians (X))));

   function Tan (X : in Degrees) return Celestial_Real is
      (Celestial_Elementry.Tan (Celestial_Real (To_Radians (X))));

   function Arcsin (X : in Celestial_Real) return Degrees is
      (To_Degrees (Radians (Celestial_Elementry.Arcsin (X))));

   function Arccos (X : in Celestial_Real) return Degrees is
      (To_Degrees (Radians (Celestial_Elementry.Arccos (X))));

   function Arctan (Y : in Celestial_Real;
                    X : in Celestial_Real := 1.0) return Degrees is
      (To_Degrees (Radians (Celestial_Elementry.Arctan (Y, X))));

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

   function Sqrt (X : Celestial_Real) return Celestial_Real renames
     Celestial_Elementry.Sqrt;

   Function To_Radians (X : in Degrees) return Radians is
     (Two_Pi * Radians (X / Degrees (Full_Circle)));

   function To_Degrees (X : in Radians) return Degrees is

      -- Reduces input value to range  - Two_Pi .. Two_Pi before conversion for
      -- large values there there will be a loss of percision

      Temp : Radians := X;

   begin -- To_Degrees
      if X < -Two_Pi or Two_Pi < X then
         Temp := X - (Radians'Truncation (X / Two_Pi) * Two_Pi);
      end if;
      return Degrees (Temp / Two_Pi) * Degrees (Full_Circle);
   end To_Degrees;

end Celestial;
