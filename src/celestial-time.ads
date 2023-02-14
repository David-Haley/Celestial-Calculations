-- This packakage is intended to support conversions from time as defined by
-- Ada.Calendar to and from Julian Day Greenwich Siderial Time etc.
-- algorithms are based on Celestial Calculations by J L Lawrence.
-- All date calculations assumes Gregorian dates which will yeild a result in
-- the range of Ada Year_Number type. In general an exception will be raised if
-- a year outside this renge is used.

-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 14/02/2023

-- 20230214 : Types Dates and Times added.
-- 20230204 : Longitude declaration Changed.
-- 20230102 : Ada.Calendar.Time_Zones.Time_Offset versions of To_Greenwich and
-- TO_Local added.
-- 20230131 : Year_Days and Day_of_Year exported.

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Celestial; use Celestial;

package Celestial.Time is

   type Dates is record
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
   end record; -- Dates

   type Times is record
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
   end record; -- Times

   subtype Year_Days is Positive range 1 .. 366;

   Function To_Hours (Time : in Times) return Decimal_Hours;

   function To_Hours return Decimal_Hours;
   -- Returns UTC as a decimal.

   function To_HHMMSS (Decimal_Hour_In : in Decimal_Hours) return Times;

   -- Conversion reduces the range to 00:00:00 .. 23:59:59, nehative values are
   -- have 24.0 hours added. 24.0 converts to 00:00:00

   function Julian_Day (Date : in Dates;
                        Time : in Times) return Julian_Days;

   function Julian_Day return Julian_Days;

   procedure From_Julian_Day (Julian_Day : in Julian_Days;
                              Date : out Dates;
                              Time : out Times);

   function From_Julian_Day (Julian_Day : in Julian_Days)
                             return Ada.Calendar.Time;

   function Day_of_Year (Date : in Dates) return Year_Days;
   -- Days counting 1 January of year as 1

   function UTC_To_GST (Date : in Dates; Time : in Times) return Decimal_Hours;

   function UTC_To_GST return Decimal_Hours;

   function GST_To_UTC (Date : in Dates; Time : in Times) return Decimal_Hours;

   function To_Time_Offset (Longitude : in Longitudes) return Time_Offsets;
   -- Note returns Celestial.Time_Offsets


   function To_Time_Offset (Longitude : in Longitudes) return Time_Offset;
   -- Note returns Ada.Calendar.Time_Zones.Time_Offset

   procedure To_Greenwich (Offset : in Time_Offset;
                           Date : in out Dates;
                           Time : in out Times);
   -- Uses Ada.Calendar.Formatting

   procedure To_Greenwich (Offset : in Time_Offsets;
                           Date : in out Dates;
                           Decimal_Hour : in out Decimal_Hours);
   -- Uses Celestial.Time_Offsets which provides higher resolution than the Ada
   -- equivalemt which has one minute resolution or 15' of arc.

   procedure To_Local (Offset : in Time_Offset;
                       Date : in out Dates;
                       Time : in out Times);

   -- Uses Ada.Calendar.Formatting

   procedure To_Local  (Offset : in Time_Offsets;
                        Date : in out Dates;
                        Decimal_Hour : in out Decimal_Hours);
   -- Uses Celestial.Time_Offsets which provides higher resolution than the Ada
   -- equivalemt which has one minute resolution or 15' of arc.

end Celestial.Time;
