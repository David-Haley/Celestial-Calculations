-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 04/01/2023

with Celestial; use Celestial;

package body Celestial.Coordinates is

   subtype C_R is Celestial_Real;

   function To_Latitude (Angle : in Radians) return Latitudes is

      -- Will raise exception if Angle outside of range -Half_Pi .. Half_Pi.

      Result : Latitudes;

   begin -- To_Latitude
      Result.Angle := abs (To_Degrees (Angle));
      if Angle >= 0.0 then
         Result.Hemisphere := North;
      else
         Result.Hemisphere := South;
      end if; -- Angle < 0.0
      return Result;
   end To_Latitude;

   function To_Latitude (Angle : in Degrees) return Latitudes is

      -- Will raise exception if Angle outside of range
      -- -Full_Circle / 4 .. Full_Circle / 4

      Result : Latitudes;

   begin -- To_Latitude
      Result.Angle := abs (Angle);
      if Angle >= 0.0 then
         Result.Hemisphere := North;
      else
         Result.Hemisphere := South;
      end if; -- Angle < 0.0
      return Result;
   end To_Latitude;

   function To_Longitude (Angle : in Radians) return Longitudes is

      -- Will raise exception if angle outside of range -Pi to Pi.

      Result : Longitudes;

   begin -- To_Longitude
      Result.Angle := abs (To_Degrees (Angle));
      if Angle >= 0.0 then
         Result.Hemisphere := East;
      else
         Result.Hemisphere := West;
      end if; -- Angle < 0.0
      return Result;
   end To_Longitude;

   function To_Longitude (Angle : in Degrees) return Longitudes is

      -- Will raise exception if angle outside of range
      -- -Full_Circle / 2 .. Full_Circle / 2.

      Result : Longitudes;

   begin -- To_Longitude
      Result.Angle := abs (Angle);
      if Angle >= 0.0 then
         Result.Hemisphere := East;
      else
         Result.Hemisphere := West;
      end if; -- Angle < 0.0
      return Result;
   end To_Longitude;

   function To_Longitude (Time_Offset : in Time_Offsets) return Longitudes is

      -- Converts from a time offset in decimal hours relative to UTC to
      -- Longitude in degrees.

      Result : Longitudes;

   begin -- To_Longitude
      Result.Angle := Degrees (abs (C_R (Time_Offset) / C_R (Full_Day))) *
        Degrees (Full_Circle);
      if Time_Offset >= 0.0 then
         Result.Hemisphere := East;
      else
         Result.Hemisphere := West;
      end if; -- Time_Offset >= 0.0
      return Result;
   end To_Longitude;

end Celestial.Coordinates;
