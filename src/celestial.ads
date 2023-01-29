-- Type declarations for Celestial
-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 26/03/2020

with Ada.Numerics;

package Celestial is

   -- full precision real
   type Celestial_Real is digits 15;

   type Radians is new Celestial_Real;

   Two_Pi : Radians := 2.0 * Ada.Numerics.Pi;
   Half_Pi : Radians := Ada.Numerics.Pi / 2.0;

   -- time related types
   subtype Decimal_Hours is Celestial_Real range -24.0 .. 24.0;
   subtype Time_Offsets is Decimal_Hours range -12.0 .. 12.0;
   -- Not to be confused with Ada.Calendar.Time_Zones.Time_Offset
   subtype Julian_Days is  Celestial_Real;

   -- coordinate related types
   subtype Degrees is Celestial_Real range -360.0 .. 360.0;
   subtype Degrees_I is Integer range -360 .. 360;
   subtype Minutes_N is Integer range 0 .. 60;
   subtype Seconds_N is Minutes_N;
   subtype Semis is Degrees range 0.0 .. 180.0;
   subtype Quadrents is Degrees range 0.0 .. 90.0;
   type Directions is (North, South, East, West);
   subtype Longitude_Directions is Directions range East .. West;
   subtype Latitude_Directions is Directions range North .. South;

   function sin (X : in Radians) return Celestial_Real;
   function cos (X : in Radians) return Celestial_Real;
   function tan (X : in Radians) return Celestial_Real;

   function arcsin (X : in Celestial_Real) return Radians;
   function arccos (X : in Celestial_Real) return Radians;
   function arctan (Y : in Celestial_Real; X : in Celestial_Real := 1.0)
                    return Radians;

end Celestial;
