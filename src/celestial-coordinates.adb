-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 26/03/2020

-- with Ada.Calendar; use Ada.Calendar;
--  with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
--  with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Text_IO; use Ada.Text_IO;
with Celestial; use Celestial;

package body Celestial.Coordinates is

   function To_Radians (Angle : in Degrees) return Radians is

   begin -- To_Radians
      return Radians (Angle / 360.0) * Two_Pi;
   end To_Radians;

   function To_Degrees (Angle : in Radians) return Degrees is

      -- May return incorrect results for very large values of Angle.

      Temp : Radians := abs (Angle);

   begin -- To_Degrees
      if Temp > 100.0 * Two_Pi then
         -- 100 is arbitary number to allow this function to return a result
         -- even when the subtraction of Two_Pi from Temp would not change the
         -- value of Temp. Obviously a large number of subtractions would also
         -- be slow. There is the potential for serious loss of precision if
         -- Angle is very large. For Angle of the order of 10 ** 13 there
         -- would only be two significant digits.
         Temp := Temp - Radians (Radians'Truncation (Temp / Two_Pi)) * Two_Pi;
      else
         while Temp > Two_Pi loop
            Temp := Temp - Two_Pi;
         end loop; -- Temp > Two_Pi
      end if; -- Temp > 100.0 * Two_Pi
      if Angle >= 0.0 then
         return Degrees (Temp / Two_Pi * 360.0);
      else
         return - Degrees (Temp / Two_Pi * 360.0);
      end if; -- Angle >= 0.0
   end To_Degrees;

   procedure To_Latitude (Angle : in Radians; Degree : out Semis;
                          Hemisphere : out Latitude_Directions) is

      -- May return incorrect results for very large values of Angle.

   begin -- To_Latitude
      Degree := abs (To_Degrees (Angle));
      if Angle >= 0.0 then
         Hemisphere := North;
      else
         Hemisphere := South;
      end if; -- Angle < 0.0
   end To_Latitude;

   procedure To_Longitude (Angle : in Radians; Degree : out Semis;
                           Hemisphere : out Longitude_Directions) is

      -- May return incorrect results for very large values of Angle.

      Temp : Radians := abs (Angle);

   begin -- To_Longitude
      Degree := abs (To_Degrees (Angle));
      if Angle >= 0.0 then
         Hemisphere := East;
      else
         Hemisphere := West;
      end if; -- Angle < 0.0
   end To_Longitude;

   function To_Longitude (Time_Offset : in Time_Offsets)
                          return Degrees is

      -- Converts from a time offset in decimal hours relative to UTC to
      -- Longitude in degrees.

   begin -- To_Longitude
      return Time_Offset * 15.0;
   end To_Longitude;

end Celestial.Coordinates;
