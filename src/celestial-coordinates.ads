-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 04/02/2023

-- 20230204 : To_Degrees and To_Radians transferred to Celestial;

with Celestial; use Celestial;

package Celestial.Coordinates is

   function To_Latitude (Angle : in Radians) return Latitudes;
   -- Will raise exception if Angle outside of range -Half_Pi .. Half_Pi.

   function To_Latitude (Angle : in Degrees) return Latitudes;
   -- Will raise exception if Angle outside of range
   -- -Full_Circle / 4 .. Full_Circle / 4

   function To_Longitude (Angle : in Radians) return Longitudes;
   -- Will raise exception if angle outside of range -Pi to Pi.

   function To_Longitude (Angle : in Degrees) return Longitudes;
   -- Will raise exception if angle outside of range
   -- -Full_Circle / 2 .. Full_Circle / 2.

   function To_Longitude (Time_Offset : in Time_Offsets) return Longitudes;
   -- Converts from a time offset in decimal hours relative to UTC to
   -- Longitude.

end Celestial.Coordinates;
