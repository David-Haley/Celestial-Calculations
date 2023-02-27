-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 27/02/2023

-- 20230227 : Signs incorrect in the equations 4.8.5 and 4.8.6, could not find
-- an error in the functions but could not get book result. Wikipedia to the
-- rescue! To_Hour_Angle and To_Right_Ascension corrected to allow for cases
-- where subtraction results in a negative result.
-- 20230226 : Concersions between Ecliptic and Equatorial coordinates added;
-- 20230218 : To_Altitude and To_Azimuth added. Correction to To_DDDMMSS to
-- round up to the nearest second.
-- 20230217 : Obliquity of the Ecliptic added.
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

      Half_Second : constant Degrees := Degrees (0.5 / C_R (Sixty * Sixty));
      Angle : Degrees := Angle_In;
      Check : Degrees;
      Result : DDDMMSSs;

   begin -- To_DDDMMSS
      if Angle < 0.0 then
         Angle := Angle + Degrees (Full_Circle);
      end if; -- Angle < 0.0
      if Angle >= Degrees (Full_Circle) then
         Angle := Angle - Degrees'Truncation (Angle / Degrees (Full_Circle)) *
           Degrees (Full_Circle);
      end if; -- Angle >= Degrees (Full_Circle)
      Check := Angle;
      Result.Degree := Degrees_N (Degrees'Truncation (Angle));
      Angle := (Angle - Degrees (Result.Degree)) * Degrees (Sixty);
      Result.Minute := Minutes_N (Degrees'Truncation (Angle));
      Angle := (Angle - Degrees (Result.Minute)) * Degrees (Sixty);
      Result.Second := Seconds_N (Degrees'Truncation (Angle));
      if Check - To_Degrees (Result) > Half_Second then
         -- Add one second to result if seconds should be rounded up
         if Result.Second < Seconds_N'Last then
            Result.Second := Result.Second + 1;
         else
            Result.Second := 0;
            if Result.Minute < Minutes_N'Last then
               Result.Minute := Result.Minute + 1;
            else
               Result.Minute := 0;
               if Result.Degree < Degrees_N'Last then
                  Result.Degree := Result.Degree + 1;
               else
                  Result.Degree := 0;
               end if; -- Result.Degree < Degrees_N'Last
            end if; -- Result.Minute < Minutes_N'Last
         end if; -- Result.Second < Seconds_N'Last
      end if; -- Check - To_Degrees (Result) > Half_Second
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

      Result : Celestial_Real;

   begin -- To_Right_Ascension
      Result := C_R (LST) - C_R (Hour_Angle);
      if Result < 0.0 then
         Result := Result + C_R (Full_Day);
      end if; -- Result < 0.0
      return Right_Ascensions (Result);
   end To_Right_Ascension;

   function To_Hour_Angle (LST : in Decimal_Hours;
                           Right_Ascension : in Right_Ascensions)
                           return Right_Ascensions is

      Result : Celestial_Real;

   begin -- To_Hour_Angle
      Result := C_R (LST) - C_R (Right_Ascension);
      if Result < 0.0 then
         Result := Result + C_R (Full_Day);
      end if; -- Result < 0.0
      return Right_Ascensions (Result);
   end To_Hour_Angle;

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

   -- Convert from Equatorial to Horizon given Declination, Hour Angle and
   -- Latitude, calculate Altitude and Azimuth.

   function To_Altitude (Declination : in Declinations;
                         Hour_Angle : in Right_Ascensions;
                         Latitude : in Latitudes) return Altitudes is

      Lat : constant Degrees := To_Angle (Latitude);
      H_A : constant Degrees :=
        Degrees (C_R (Hour_Angle) / C_R (Full_Day) * C_R (Full_Circle));

   begin -- To_Altitude
      return Arcsin (Sin (Declination) * Sin (Lat) +
                       Cos (Declination) * Cos (Lat) * Cos (H_A));
   end To_Altitude;

   function To_Azimuth (Declination : in Declinations;
                        Hour_Angle : in Right_Ascensions;
                        Latitude : in Latitudes) return Azimuths is

      Lat : constant Degrees := To_Angle (Latitude);
      H_A : constant Degrees :=
        Degrees (C_R (Hour_Angle) / C_R (Full_Day) * C_R (Full_Circle));
      Alt : constant Degrees := To_Altitude (Declination, Hour_Angle, Latitude);

   begin -- To_Azimuth
      if Sin (H_A) >= 0.0 then
         return Degrees (Full_Circle) - Arccos ((Sin (Declination) -
                                                  Sin (Lat) * Sin (Alt)) /
                                                  (Cos (Lat) * Cos (Alt)));
      else
         return Arccos ((Sin (Declination) - Sin (Lat) * Sin (Alt)) /
                          (Cos (Lat) * Cos (Alt)));
      end if; -- Sin (H_A) >= 0.0
   end To_Azimuth;

   -- Required for conversions to and from Ecliptic coordinates.

   function Obliquity_Ecliptic (Date : in Dates) return Degrees is

      -- Obliquity of the Ecliptic Uses JPL equation,
      -- valid for dates after 1 January 2000.

      E0 : constant Celestial_Real :=
        23.0 + (21.448 / C_R (Sixty) + 26.0) / C_R (Sixty);
      E1 : constant Celestial_Real := -46.815 / C_R (Sixty) / C_R (Sixty);
      E2 : constant Celestial_Real := -0.00059 / C_R (Sixty) / C_R (Sixty);
      E3 : constant Celestial_Real := 0.001813 / C_R (Sixty) / C_R (Sixty);
      T : Celestial_Real;

   begin -- Obliquity_Ecliptic
      T := C_R (Julian_Day (Date, (0, 0, 0)) -
                  Julian_Day ((2000, 1, 1), (12, 0, 0))) / 36525.0;
      -- T is Julian centuries since 12:00:00 on 01/01/2000
      return Degrees (E0 + E1 * T + E2 * T ** 2 + E3 * T ** 3);
   end Obliquity_Ecliptic;

   -- Convert from Ecliptic to Equatorial given Ecliptic Latitude,
   -- Ecliptic Longitude and Date, the latter is required to calculate the
   -- Obliquity Ecliptic.

   function To_Declination (Ecliptic_Latitude : in Ecliptic_Latitudes;
                            Ecliptic_Longitude : in Ecliptic_Longitudes;
                            Date : in Dates) return Declinations is

      E : constant Degrees := Obliquity_Ecliptic (Date);
      Declination : Declinations;

   begin -- To_Declination
      Declination := Arcsin (Sin (Ecliptic_Latitude) * Cos (E) +
                               Cos (Ecliptic_Latitude) * Sin (E) *
                               Sin (Ecliptic_Longitude));
      return Declination;
   end To_Declination;

   function To_Right_Ascension (Ecliptic_Latitude : in Ecliptic_Latitudes;
                                Ecliptic_Longitude : in Ecliptic_Longitudes;
                                Date : in Dates) return Right_Ascensions is

      E : constant Degrees := Obliquity_Ecliptic (Date);
      X, Y : Celestial_Real;
      A : Degrees;

   begin -- To_Right_Ascension
      Y := Sin (Ecliptic_Longitude) * Cos (E) -
        Tan (Ecliptic_Latitude) * Sin (E);
      X := Cos (Ecliptic_Longitude);
      if X >= 0.0 then
         if Y >= 0.0 then
            A := Arctan (Y / X);
         else
            A := Arctan (Y / X) + 180.0;
         end if; -- Y >= 0.0
      else
         if Y >= 0.0 then
            A := Arctan (Y / X) + 360.0;
         else
            A := Arctan (Y / X) + 180.0;
         end if; -- Y >= 0.0
      end if; -- X >= 0.0
      return Right_Ascensions (C_R (A) * C_R (Full_Day) / C_R (Full_Circle));
   end To_Right_Ascension;

   -- Convert from Equatorial to Ecliptic given  Right Ascension, Declination
   -- and Date, the latter is required to calculate the Obliquity Ecliptic.

   function To_Ecliptic_Latitude (Declination : in Declinations;
                                  Right_Ascension : in Right_Ascensions;
                                  Date : in Dates) return Ecliptic_Latitudes is

      E : constant Degrees := Obliquity_Ecliptic (Date);
      A : constant Degrees := Degrees (C_R (Right_Ascension) /
                                         C_R (Full_Day) * C_R (Full_Circle));
      -- A is Degrees not Decimal_Hours
      Ecliptic_Latitude : Degrees;

   begin -- To_Ecliptic_Latitude
      Ecliptic_Latitude := Arcsin (Sin (Declination) * Cos (E) -
                                     Cos (Declination) * Sin (E) * Sin (A));
      return Ecliptic_Latitude;
   end To_Ecliptic_Latitude;

   function To_Ecliptic_Longitude (Declination : in Declinations;
                                   Right_Ascension : in Right_Ascensions;
                                   Date : in Dates)
                                   return Ecliptic_Longitudes is

      E : constant Degrees := Obliquity_Ecliptic (Date);
      A : constant Degrees := Degrees (C_R (Right_Ascension) /
                                         C_R (Full_Day) * C_R (Full_Circle));
      -- A is Degrees not Decimal_Hours
      X, Y : Celestial_Real;
      Ecliptic_Longitude : Degrees;

   begin -- To_Ecliptic_Longitude
      Y := Sin (A) * Cos (E) + Tan (Declination) * Sin (E);
      X := Cos (A);
      if X >= 0.0 then
         if Y >= 0.0 then
            Ecliptic_Longitude := Arctan (Y / X);
         else
            Ecliptic_Longitude := Arctan (Y / X) + 180.0;
         end if; -- Y >= 0.0
      else
         if Y >= 0.0 then
            Ecliptic_Longitude := Arctan (Y / X) + 360.0;
         else
            Ecliptic_Longitude := Arctan (Y / X) + 180.0;
         end if; -- Y >= 0.0
      end if; -- X >= 0.0
      return Ecliptic_Longitude;
   end To_Ecliptic_Longitude;

   procedure Precession_Correction (Declination_Old : in Declinations;
                                    Right_Ascension_Old : in Right_Ascensions;
                                    Epoch_Old, Epoch_New : in Dates;
                                    Declination_New : out Declinations;
                                    Right_Ascension_New : out Right_Ascensions)
   is

      A : constant Degrees := Degrees (C_R (Right_Ascension_Old) /
                                         C_R (Full_Day) * C_R (Full_Circle));
      D, M, Nd, Nt, T : Celestial_Real;

   begin -- Precession_Correction;
      D := C_R (Julian_Day (Epoch_New, (0, 0, 0)) -
                  Julian_Day ((Epoch_Old), (0, 0, 0))) / 365.25;
      -- Difference in years
      T := (C_R (Epoch_New.Year) - 1900.0) / 100.0; -- Centuries since 1900
      M := (3.072434 + 0.00186 * T) / C_R (Sixty * Sixty);
      -- seconds converted to Hours
      Nd := (20.0468 - 0.0085 * T) / C_R (Sixty * Sixty);
      -- Arc seconds converted degrees
      Nt := Nd / 15.0;
      Declination_New := Declination_Old + Declinations (Nd * Cos (A) * D);
      Right_Ascension_New := Right_Ascension_Old +
        Right_Ascensions ((M + Nt * (Sin (A) * Tan (Declination_Old))) * D);
   end Precession_Correction;

end Celestial.Coordinates;
