with Ada.Calendar.Time_Zones;
-- This packakage is intended to support conversions from time as defined by
-- Ada.Calendar to and from Julian Day Greenwich Siderial Time etc.
-- algorithms are based on Celestial Calculations by J L Lawrence.
-- All date calculations assumes Gregorian dates which will yeild a result in
-- the range of Ada Year_Number type. In general an exception will be raised if
-- a year outside this renge is used.
-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 27/11/2019

-- with Ada.Text_IO; use Ada.Text_IO;

package body Celestial.Time is

   Function To_Hours (Hour : in Hour_Number;
                      Minute : in Minute_Number;
                      Second : in Second_Number) return Decimal_Hours is
   begin -- To_Hours
      return Decimal_Hours (Hour) +
        (Celestial_Real (Minute) +
             Celestial_Real (Second) / Celestial_Real (Second_Number'Last + 1))
          / Celestial_Real (Minute_Number'Last + 1);
   end To_Hours;

   function To_Hours return Decimal_Hours is
      -- Returns uniform Time as a decimal.

      Now : Ada.Calendar.Time := Clock;

   begin -- To_Hours
      return To_Hours (Ada.Calendar.Formatting.Hour (Now),
                       Ada.Calendar.Formatting.Minute (Now),
                       Ada.Calendar.Formatting.Second (Now));
   end To_Hours;

   procedure To_HHMMSS (Decimal_Hour : in Decimal_Hours;
                        Hour : out Hour_Number;
                        Minute : out Minute_Number;
                        Second : out Second_Number) is

      -- conversion is the based on abs (Decimal_Hours)

      This_Hour : Natural :=
        Natural (Celestial_Real'Rounding (abs (Decimal_Hour) * 3600.0));
      Day : constant Natural := 24 * 3600;

   begin -- To_HHMMSS
      -- Rounding could result in This_Time equal to 24:00:00
      if This_Hour >= Day then
         This_Hour := Day - 1;
      end if; --
      Second := Second_Number (This_Hour mod 60);
      This_Hour := This_Hour / 60;
      Minute := Minute_Number (This_Hour mod 60);
      This_Hour := This_Hour / 60;
      Hour := Hour_Number (This_Hour);
   end To_HHMMSS;

   function Julian_Day (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Julian_Days is

      Valid_Time : Ada.Calendar.Time
        := Time_Of (Year, Month, Day, Hour, Minute, Second);
      -- will raise exception if not valid Gregorian date
      This_Year : Julian_Days := Julian_Days (Year);
      This_Month : Julian_Days := Julian_Days (Month);
      This_Day : Julian_Days;
      A, B : Integer;

   begin -- Julian_Day
      This_Day := Julian_Days (Day) +
        To_Hours (Hour, Minute, Second) / Julian_Days (Hour_Number'Last + 1);
      if Month <= 2 then
         This_Month := This_Month + 12.0;
         This_Year := This_Year - 1.0;
      end if; -- Month <= 2
      A := Integer (Julian_Days'Truncation (This_Year / 100.0));
      B := 2 - A + A / 4;
      return Julian_Days (B) + Julian_Days'Truncation (This_Year * 365.25) +
        Julian_Days'Truncation ((This_Month + 1.0) * 30.6001) +
        This_Day + 1720994.5;
   end Julian_Day;

   function Julian_Day return Julian_Days is

      Now : Ada.Calendar.Time := Clock;

   begin -- Julian_Day
      return Julian_Day (Ada.Calendar.Formatting.Year (Now),
                         Ada.Calendar.Formatting.Month (Now),
                         Ada.Calendar.Formatting.Day (Now),
                         Ada.Calendar.Formatting.Hour (Now),
                         Ada.Calendar.Formatting.Minute (Now),
                         Ada.Calendar.Formatting.Second (Now));
   end Julian_Day;

   procedure From_Julian_Day (Julian_Day : in Julian_Days;
                              Year : out Year_Number;
                              Month : out Month_Number;
                              Day : out Day_Number;
                              Hour : out Hour_Number;
                              Minute : out Minute_Number;
                              Second : out Second_Number) is

      This_Julian_Day : Julian_Days := Julian_Day + 0.5;
      Day_Int : Julian_Days;
      A, B, C, D, E, G : Integer;
      Decimal_Hour : Decimal_Hours;

   begin -- From_Julian_Day
      Day_Int := Julian_Days'Truncation (This_Julian_Day);
      Decimal_Hour := Decimal_Hours ((This_Julian_Day - Julian_Days(Day_Int))
                                     * 24.0);
      To_HHMMSS (Decimal_Hour, Hour, Minute, Second);
      A := Integer (Julian_Days'Truncation ((Day_Int - 1867216.25) / 36524.25));
      B := Integer (Day_Int) + 1 + A - (A / 4);
      C := B + 1524;
      D :=
        Integer (Julian_Days'Truncation ((Julian_Days (C) - 122.1) / 365.25));
      E := Integer (Julian_Days'Truncation (Julian_Days (D) * 365.25));
      G := Integer (Julian_Days'Truncation (Julian_Days (C - E) / 30.6001));
      Day := C - E -
        Integer (Julian_Days'Truncation (Julian_Days (G) * 30.6001));
      if G > 13 then
         Month := G - 13;
      else
         Month := G - 1;
      end if; -- G > 13
      if Month > 2 then
         Year := D - 4716;
      else
         Year := D - 4715;
      end if; -- Month > 2;
   end From_Julian_Day;

   function From_Julian_Day (Julian_Day : in Julian_Days)
                             return Ada.Calendar.Time is

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- From_Julian_Day
      From_Julian_Day (Julian_Day, Year, Month, Day, Hour, Minute, Second);
      return Time_Of (Year, Month, Day, Hour, Minute, Second);
   end From_Julian_Day;

   function UTC_To_GST (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Decimal_Hours is

      Day_of_Year, T, R, B : Julian_Days;
      T0, Result : Decimal_Hours;

   begin -- UTC_To_GST
      Day_of_Year :=
        Julian_Days'Rounding (Julian_Day (Year, Month, Day, 0, 0, 0)
                              - Julian_Day (Year, 1, 1, 0, 0, 0));
      -- Days since 1 January of year, as an integer
      T := (Julian_Day (Year, 1, 1, 0, 0, 0) - 2415020.0) / 36525.0;
      R := 6.6460656 + 2400.051262 * T + 0.00002581 * T ** 2;
      B := 24.0 - R + 24.0 * Julian_Days (Year - 1900);
      T0 := Decimal_Hours (0.0657098 * Day_of_Year - B);
      Result := T0 + 1.002738 * To_Hours (Hour, Minute, Second);
      if Result < 0.0 then
         return Result + 24.0;
      else
         return Result;
      end if; -- Result < 0.0
   end UTC_To_GST;

   function UTC_To_GST return Decimal_Hours is

      Now : Ada.Calendar.Time := Clock;

   begin -- UTC_To_GST
      return UTC_To_GST (Ada.Calendar.Formatting.Year (Now),
                         Ada.Calendar.Formatting.Month (Now),
                         Ada.Calendar.Formatting.Day (Now),
                         Ada.Calendar.Formatting.Hour (Now),
                         Ada.Calendar.Formatting.Minute (Now),
                         Ada.Calendar.Formatting.Second (Now));
   end UTC_To_GST;

   function GST_To_UTC (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Decimal_Hours is

      Day_of_Year, T, R, B, T0, A : Julian_Days;

   begin -- GST_To_UTC
      Day_of_Year :=
        Julian_Days'Rounding (Julian_Day (Year, Month, Day, 0, 0, 0)
                              - Julian_Day (Year, 1, 1, 0, 0, 0));
      -- Days since 1 January of year, as an integer
      T := (Julian_Day (Year, 1, 1, 0, 0, 0) - 2415020.0) / 36525.0;
      R := 6.6460656 + 2400.051262 * T + 0.00002581 * T ** 2;
      B := 24.0 - R + 24.0 * Julian_Days (Year - 1900);
      T0 := Julian_Days (0.0657098 * Day_of_Year - B);
      if T0 > 24.0 then
         T0 := T0 - 24.0;
      elsif T0 < 0.0 then
         T0 := T0 + 24.0;
      end if; -- T0 > 24.0
      A := To_Hours (Hour, Minute, Second) - T0;
      if A < 0.0 then
         A := A + 24.0;
      end if; -- A < 0.0
      return Decimal_Hours (0.997270 * A);
   end GST_To_UTC;

   function To_Time_Offset (Longitude : in Semis;
                            Direction : in Longitude_Directions)
                            return Time_Offsets is

      Per_Degree : constant Time_Offsets := 24.0 / 360.0;

   begin
      if Direction = East then
         return Time_Offsets (Longitude * Per_Degree);
      else
         return Time_Offsets (-Longitude * Per_Degree);
      end if; -- Direction = West
   end To_Time_Offset;

   function To_Time_Offset (Longitude : in Semis;
                            Direction : in Longitude_Directions)
                            return Ada.Calendar.Time_Zones.Time_Offset is

      Hour_Offset : Time_Offsets := To_Time_Offset (Longitude, Direction);

   begin -- To_Time_Offset
      return Time_Offset (Celestial_Real'Rounding (Hour_Offset * 60.0));
   end To_Time_Offset;

   function To_Greenwich (Decimal_Hour : in Decimal_Hours;
                          Offset : in Time_Offsets) return Decimal_Hours is

      Result : Celestial_Real := Decimal_Hour - Offset;

   begin -- To_Greenwich
      if Result < 0.0 then
         Result := Result + 24.0;
      elsif Result >= 24.0 then
         Result := Result - 24.0;
      end if; -- Result < 0.0
      return Result;
   end To_Greenwich;

   function To_Local (Decimal_Hour : in Decimal_Hours;
                      Offset : in Time_Offsets) return Decimal_Hours is

      Result : Celestial_Real := Decimal_Hour + Offset;

   begin -- To_Local
      if Result < 0.0 then
         Result := Result + 24.0;
      elsif Result >= 24.0 then
         Result := Result - 24.0;
      end if; -- Result < 0.0
      return Result;
   end To_Local;

end Celestial.Time;
