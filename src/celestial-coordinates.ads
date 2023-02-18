-- This packakage is intended to support conversions between various coordinate
-- systems. Algorithms are based on Celestial Calculations by J L Lawrence.
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 17/02/2023

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

   function Hour_Angle (LST : in Decimal_Hours;
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
   -- Obliquity of the Ecliptic Uses JPL equation,
   -- valid for dates after 1 January 2000.

end Celestial.Coordinates;
