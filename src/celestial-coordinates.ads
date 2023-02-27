-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 27/02/2023

-- 20230227 : Precession_Correction added, Hour_Angle changed to To_Hour_Angle.
-- 20230226 : Concersions between Ecliptic and Equatorial coordinates added;
-- 20230217 : To_Altitude, To_Azimuth and Obliquity_Ecliptic added.
-- 20230206 : functions releted to Hour Angle and Right Ascension added.
-- 20230204 : To_Degrees and To_Radians transferred to Celestial;

with Celestial; use Celestial;
with Celestial.Time; use Celestial.Time;

package Celestial.Coordinates is

   function To_Degrees (DDDMMSS : in DDDMMSSs) return Degrees;
   -- Converts degrees minutes and seconds to Decimals of degrees.

   function To_DDDMMSS (Angle_In : in Degrees) return DDDMMSSs;
   -- Reduces angle to range 0 <= Angle < 360.0 before conversion to
   -- Conversion to Degrees, Minutes and Seconds.

   function To_Latitude (Angle : in Radians) return Latitudes;
   -- Will raise exception if Angle outside of range -Half_Pi .. Half_Pi.

   function To_Angle (Latitude : in Latitudes) return Radians;

   function To_Latitude (Angle : in Degrees) return Latitudes;
   -- Will raise exception if Angle outside of range
   -- -Full_Circle / 4 .. Full_Circle / 4

   function To_Angle (Latitude : in Latitudes) return Degrees;

   function To_Longitude (Angle : in Radians) return Longitudes;
   -- Will raise exception if angle outside of range -Pi to Pi.

   function To_Angle (Longitude : Longitudes) return Radians;

   function To_Longitude (Angle : in Degrees) return Longitudes;
   -- Will raise exception if angle outside of range
   -- -Full_Circle / 2 .. Full_Circle / 2.

   function To_Angle (Longitude : Longitudes) return Degrees;

   function To_Longitude (Time_Offset : in Time_Offsets) return Longitudes;
   -- Converts from a time offset in decimal hours relative to UTC to
   -- Longitude.

   function To_Right_Ascension (LST : in Decimal_Hours;
                                Hour_Angle : in Right_Ascensions)
                                return Right_Ascensions;

   function To_Hour_Angle (LST : in Decimal_Hours;
                        Right_Ascension : in Right_Ascensions)
                        return Right_Ascensions;

   -- Conversion from Horizon to Equatorial given Altitude, Azimuth and Latitude
   -- calculate Declination and Hour Angle.

   function To_Declination (Altitude : in Altitudes;
                            Azimuth : in Azimuths;
                            Latitude : in Latitudes) return Declinations;

   function To_Hour_Angle (Altitude : in Altitudes;
                           Azimuth : in Azimuths;
                           Latitude : in Latitudes) return Right_Ascensions;

   -- Convert from Equatorial to Horizon given Declination, Hour Angle and
   -- Latitude, calculate Altitude and Azimuth.

   function To_Altitude (Declination : in Declinations;
                         Hour_Angle : in Right_Ascensions;
                         Latitude : in Latitudes) return Altitudes;

   function To_Azimuth (Declination : in Declinations;
                        Hour_Angle : in Right_Ascensions;
                        Latitude : in Latitudes) return Azimuths;

   -- Required for conversions to and from Ecliptic coordinates.

   function Obliquity_Ecliptic (Date : in Dates) return Degrees;
   -- Obliquity of the Ecliptic Uses JPL equation.

   -- Convert from Ecliptic to Equatorial given Ecliptic Latitude,
   -- Ecliptic Longitude and Date, the latter is required to calculate the
   -- Obliquity Ecliptic.

   function To_Declination (Ecliptic_Latitude : in Ecliptic_Latitudes;
                            Ecliptic_Longitude : in Ecliptic_Longitudes;
                            Date : in Dates) return Declinations;

   function To_Right_Ascension (Ecliptic_Latitude : in Ecliptic_Latitudes;
                                Ecliptic_Longitude : in Ecliptic_Longitudes;
                                Date : in Dates) return Right_Ascensions;

   -- Convert from Equatorial to Ecliptic given Right Ascension, Declination and
   -- Date, the latter is required to calculate the Obliquity Ecliptic.

   function To_Ecliptic_Latitude (Declination : in Declinations;
                                  Right_Ascension : in Right_Ascensions;
                                  Date : in Dates) return Ecliptic_Latitudes;

   function To_Ecliptic_Longitude (Declination : in Declinations;
                                   Right_Ascension : in Right_Ascensions;
                                   Date : in Dates) return Ecliptic_Longitudes;

   procedure Precession_Correction (Declination_Old : in Declinations;
                                    Right_Ascension_Old : in Right_Ascensions;
                                    Epoch_Old, Epoch_New : in Dates;
                                    Declination_New : out Declinations;
                                    Right_Ascension_New : out Right_Ascensions);

end Celestial.Coordinates;
