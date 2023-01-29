-- This packakage is intended to support conversions from time as defined by
-- Ada.Calendar to and from Julian Day Greenwich Siderial Time etc.
-- algorithms are based on Celestial Calculations by J L Lawrence.
-- All date calculations assumes Gregorian dates which will yeild a result in
-- the range of Ada Year_Number type. In general an exception will be raised if
-- a year outside this renge is used.
-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 27/11/2019

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Celestial; use Celestial;

package Celestial.Time is

   Function To_Hours (Hour : in Hour_Number;
                      Minute : in Minute_Number;
                      Second : in Second_Number) return Decimal_Hours;

   function To_Hours return Decimal_Hours;
   -- Returns UTC as a decimal.

   procedure To_HHMMSS (Decimal_Hour : in Decimal_Hours;
                        Hour : out Hour_Number;
                        Minute : out Minute_Number;
                        Second : out Second_Number);

   -- conversion is the based on abs (Decimal_Hours)

   function Julian_Day (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Julian_Days;

   function Julian_Day return Julian_Days;

   procedure From_Julian_Day (Julian_Day : in Julian_Days;
                              Year : out Year_Number;
                              Month : out Month_Number;
                              Day : out Day_Number;
                              Hour : out Hour_Number;
                              Minute : out Minute_Number;
                              Second : out Second_Number);

   function From_Julian_Day (Julian_Day : in Julian_Days)
                             return Ada.Calendar.Time;

   function UTC_To_GST (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Decimal_Hours;

   function UTC_To_GST return Decimal_Hours;

   function GST_To_UTC (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Decimal_Hours;

   function To_Time_Offset (Longitude : in Semis;
                            Direction : in Longitude_Directions)
                            return Time_Offsets;

   function To_Time_Offset (Longitude : in Semis;
                            Direction : in Longitude_Directions)
                            return Time_Offset;

   -- Note returns Ada.Calendar.Time_Zones.Time_Offset

   function To_Greenwich (Decimal_Hour : in Decimal_Hours;
                          Offset : in Time_Offsets) return Decimal_Hours;

   function To_Local (Decimal_Hour : in Decimal_Hours;
                      Offset : in Time_Offsets) return Decimal_Hours;

end Celestial.Time;
