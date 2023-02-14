-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 05/01/2023

-- 20230205 : functions releted to Hour Angle and Right Ascension added.
-- 20230204 : To_Degrees and To_Radians transferred to Celestial;

with Celestial; use Celestial;

package body Celestial.Coordinates is

   subtype C_R is Celestial_Real;

   function To_Degrees (DDDMMSS : in DDDMMSSs) return Degrees is

      -- Converts degrees minutes and seconds to Decimals of degrees.

      Result : Celestial_Real;

   begin -- To_Degrees
      Result := C_R (DDDMMSS.Degree) + (C_R (DDDMMSS.Second) / C_R (Sixty) +
                                          C_R (DDDMMSS.Minute)) / C_R (Sixty);
      return Degrees (Result);
   end To_Degrees;

   function To_DDDMMSS (Angle_In : in Degrees) return DDDMMSSs is
   -- Reduces angle to range 0 <= Angle < 360.0 before conversion to
   -- Conversion to Degrees, Minutes and Seconds.

      Angle : Degrees := Angle_In;
      Result : DDDMMSSs;

   begin -- To_DDDMMSS
      if Angle < 0.0 then
         Angle := Angle + Degrees (Full_Circle);
      end if; -- Angle < 0.0
      if Angle >= Degrees (Full_Circle) then
         Angle := Angle - Degrees'Truncation (Angle / Degrees (Full_Circle)) *
           Degrees (Full_Circle);
      end if; -- Angle >= Degrees (Full_Circle)
      Result.Degree := Degrees_N (Degrees'Truncation (Angle));
      Angle := (Angle - Degrees (Result.Degree)) * Degrees (Sixty);
      Result.Minute := Minutes_N (Degrees'Truncation (Angle));
      Angle := (Angle - Degrees (Result.Minute)) * Degrees (Sixty);
      Result.Second := Seconds_N (Degrees'Truncation (Angle));
      return Result;
   end To_DDDMMSS;

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

   function To_Angle (Latitude : in Latitudes) return Radians is

   begin -- To_Angle
      if Latitude.Hemisphere = North then
         return To_Radians (Latitude.Angle);
      else
         return -To_Radians (Latitude.Angle);
      end if; -- Latitude.Hemisphere = North
   end To_Angle;

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

   function To_Angle (Latitude : in Latitudes) return Degrees is

   begin -- To_Angle
      if Latitude.Hemisphere = North then
         return Latitude.Angle;
      else
         return -Latitude.Angle;
      end if; -- Latitude.Hemisphere = North
   end To_Angle;

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

   function To_Angle (Longitude : Longitudes) return Radians is

   begin -- To_Angle
      if Longitude.Hemisphere = East then
         return To_Radians (Longitude.Angle);
      else
         return -To_Radians (Longitude.Angle);
      end if; -- Longitude.Hemisphere = East
   end To_Angle;

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

   function To_Angle (Longitude : Longitudes) return Degrees is

   begin -- To_Angle
      if Longitude.Hemisphere = East then
         return Longitude.Angle;
      else
         return -Longitude.Angle;
      end if; -- Longitude.Hemisphere = East
   end To_Angle;

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

   function To_Right_Ascension (LST : in Decimal_Hours;
                                Hour_Angle : in Right_Ascensions)
                                return Right_Ascensions is
     (Right_Ascensions (C_R (LST) - C_R (Hour_Angle)));

   function Hour_Angle (LST : in Decimal_Hours;
                        Right_Ascension : in Right_Ascensions)
                        return Right_Ascensions is
     (Time_Offsets (C_R (LST) - C_R (Right_Ascension)));

   -- Conversion from Horizon to Equatorial given Altitude, Azimuth and Latitude
   -- calculate Declination and Hour Angle.

   function To_Declination (Altitude : in Altitudes;
                            Azimuth : in Azimuths;
                            Latitude : in Latitudes) return Declinations is

      Lat : constant Degrees := To_Angle (Latitude);

   begin -- To_Declination
      return Arcsin ( Sin (Altitude) * Sin (Lat) +
                        Cos (Altitude) * Cos (Lat) * Cos (Azimuth));
   end To_Declination;

   function To_Hour_Angle (Altitude : in Altitudes;
                           Azimuth : in Azimuths;
                           Latitude : in Latitudes) return Right_Ascensions is

      Lat : constant Degrees := To_Angle (Latitude);
      Dec : constant Declinations := To_Declination (Altitude, Azimuth,
                                                     Latitude);
      H_A_Degrees : Degrees;

   begin -- To_Hour_Angle
      H_A_Degrees := Arccos ((Sin (Altitude) - Sin (Lat) * Sin (Dec)) /
                             (Cos (Lat) * Cos (Dec)));
      if Sin (Azimuth) >= 0.0 then
         H_A_Degrees := Degrees (Full_Circle) - H_A_Degrees;
      end if; -- Sin (Azimuth) >= 0.0
      return Right_Ascensions (H_A_Degrees / Degrees (Full_Circle) *
                              Degrees (Full_Day));
   end To_Hour_Angle;

end Celestial.Coordinates;
