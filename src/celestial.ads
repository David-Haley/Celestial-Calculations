-- Type declarations for Celestial
-- the use of declarations of the form type x is new y renders x and y as
-- incompatible types this allows the compiler to detect simple errors such as
-- using the language Sin (X) which takes an argument in radians with an
-- argument that is Degrees;

-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 28/02/2023

-- 20230228 : Coordinate components made types not subtypes;
-- 20230221 : Ecliptic Coordinates added.
-- 20230206 : type DDDMMSSs added.
-- 20230203 : Pi and Other functions section added.
-- 20230131 : Decimal_Hours, Julian_Days, Radians and Degreed, made types rather
-- than sub types to render them incompatible, explicit type conversions
-- required in calculations. Degree trig functions added;

with Ada.Numerics;

package Celestial is

   -- full precision real
   type Celestial_Real is digits 15;

   type Radians is new Celestial_Real;

   Half_Pi : constant Radians := Ada.Numerics.Pi / 2.0;
   Pi : constant Radians := Ada.Numerics.Pi;
   Two_Pi : constant Radians := 2.0 * Ada.Numerics.Pi;

   Full_Day : constant := 24;
   Sixty : constant := 60;

   -- time related types
   type Decimal_Hours is new Celestial_Real range
     -Celestial_Real (Full_Day) .. Celestial_Real (Full_Day);
   subtype Time_Offsets is Decimal_Hours range
     -Decimal_Hours (Full_Day / 2) .. Decimal_Hours (Full_Day / 2);
   -- Not to be confused with Ada.Calendar.Time_Zones.Time_Offset
   type Julian_Days is new Celestial_Real range 0.0 .. Celestial_Real'Last;

   -- coordinate related types
   Full_Circle : constant := 360;
   type Degrees is new Celestial_Real range
     - Celestial_Real (Full_Circle) .. Celestial_Real (Full_Circle);
   subtype Degrees_N is Natural range 0 .. Full_Circle - 1;
   subtype Minutes_N is Natural range 0 .. Sixty - 1;
   subtype Seconds_N is Natural range 0 .. Sixty - 1;
   type DDDMMSSs is Record
      Degree : Degrees_N;
      Minute : Minutes_N;
      Second : Seconds_N;
   end record; -- DDDMMSSs
   subtype Semis is Degrees range 0.0 .. Degrees (Full_Circle / 2);
   subtype Quadrents is Degrees range 0.0 .. Degrees (Full_Circle / 4);

   -- Terestrial Coordinates
   type Directions is (North, South, East, West);
   subtype Longitude_Directions is Directions range East .. West;
   subtype Latitude_Directions is Directions range North .. South;
   -- Latitude and Longitude have similar declarations but are not compatible
   type Latitudes is record
      Hemisphere : Latitude_Directions;
      Angle : Quadrents;
   end record; -- Latitudes
   type Longitudes is record
      Hemisphere : Longitude_Directions;
      Angle : Semis;
   end record; -- Longitudes

   -- Equatorial Coordinates
   type Declinations is new Degrees range
     -Degrees (Full_Circle / 4) .. Degrees (Full_Circle / 4);
   type Right_Ascensions is new Decimal_Hours range 0.0 .. Decimal_Hours'Last;

   -- Horizon Coordinates
   type Azimuths is new Degrees range 0.0 .. Degrees (Full_Circle);
   type Altitudes is new Degrees range
     -Degrees (Full_Circle / 4) .. Degrees (Full_Circle / 4);

   -- Ecliptic Coordinates
   type Ecliptic_Latitudes is new Degrees range
     -Degrees (Full_Circle / 4) .. Degrees (Full_Circle / 4);
   type Ecliptic_Longitudes is new Degrees range 0.0 .. Degrees (Full_Circle);

   -- Trig Functions overloaded for Degrees not Radians

   function Sin (X : in Degrees) return Celestial_Real;
   function Cos (X : in Degrees) return Celestial_Real;
   function Tan (X : in Degrees) return Celestial_Real;

   function Arcsin (X : in Celestial_Real) return Degrees;
   function Arccos (X : in Celestial_Real) return Degrees;
   function Arctan (Y : in Celestial_Real;
                    X : in Celestial_Real := 1.0) return Degrees;

   -- Trig Functions overloaded for Radians not Degrees

   function Sin (X : in Radians) return Celestial_Real;
   function Cos (X : in Radians) return Celestial_Real;
   function Tan (X : in Radians) return Celestial_Real;

   function Arcsin (X : in Celestial_Real) return Radians;
   function Arccos (X : in Celestial_Real) return Radians;
   function Arctan (Y : in Celestial_Real;
                    X : in Celestial_Real := 1.0) return Radians;

   -- Other Functions

   function Sqrt (X : Celestial_Real) return Celestial_Real;

   Function To_Radians (X : in Degrees) return Radians;

   function To_Degrees (X : in Radians) return Degrees;
   -- Reduces input value to - Two_Pi .. Two_Pi before conversion for large
   -- values there there will be a loss of percision

end Celestial;
