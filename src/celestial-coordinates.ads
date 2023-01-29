-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 26/03/2020

with Celestial; use Celestial;

package Celestial.Coordinates is

   function To_Radians (Angle : in Degrees) return Radians;

   function To_Degrees (Angle : in Radians) return Degrees;

   -- May return incorrect results for very large values of Angle.

   procedure To_Latitude (Angle : in Radians; Degree : out Semis;
                          Hemisphere : out Latitude_Directions);

   -- May return incorrect results for very large values of Angle.

   procedure To_Longitude (Angle : in Radians; Degree : out Semis;
                           Hemisphere : out Longitude_Directions);

   -- May return incorrect results for very large values of Angle.

   function To_Longitude (Time_Offset : in Time_Offsets)
                          return Degrees;

   -- Converts from a time offset in decimal hours relative to UTC to
   -- Longitude in degrees.

end Celestial.Coordinates;
