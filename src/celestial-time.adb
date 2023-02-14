-- This packakage is intended to support conversions from time as defined by
-- Ada.Calendar to and from Julian Day Greenwich Siderial Time etc.
-- algorithms are based on Celestial Calculations by J L Lawrence.
-- All date calculations assumes Gregorian dates which will yeild a result in
-- the range of Ada Year_Number type. In general an exception will be raised if
-- a year outside this renge is used.

-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 14/02/2023

-- 20230214 : Typs Dates and Times added.
-- 20230204 : Longitude declaration Changed.
-- 20230102 : Ada.Calendar.Time_Zones.Time_Offset versions of To_Greenwich and
-- TO_Local added.
-- 20230130 : Consolidation of constants some functions converted to function
-- statements.

with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;

package body Celestial.Time is
   subtype C_R is Celestial_Real;
   subtype D_H is Decimal_Hours;
   subtype J_D is Julian_Days;

   Hour_Seconds_N : constant := Sixty * Sixty;
   Hour_Seconds : constant C_R := C_R (Hour_Seconds_N);
   Day_Seconds_N : constant := Full_Day * Hour_Seconds_N;
   Day_Seconds : constant C_R := C_R (Day_Seconds_N);

   Function To_Hours (Time : in Times) return Decimal_Hours is
     (D_H (C_R (Time.Hour) + (C_R (Time.Minute) +
        C_R (Time.Second) / C_R (Sixty)) / C_R (Sixty)));

   function To_Hours return Decimal_Hours is
      -- Returns uniform Time as a decimal.

      Now : Ada.Calendar.Time := Clock;
      Time : Times;

   begin -- To_Hours
      Time := (Ada.Calendar.Formatting.Hour (Now),
               Ada.Calendar.Formatting.Minute (Now),
               Ada.Calendar.Formatting.Second (Now));
      return To_Hours (Time);
   end To_Hours;

   function To_HHMMSS (Decimal_Hour_In : in Decimal_Hours) return Times is

   -- Conversion reduces the range to 00:00:00 .. 23:59:59, nehative values are
   -- have 24.0 hours added. 24.0 converts to 00:00:00

      Decimal_Hour : Celestial_Real := C_R (Decimal_Hour_In);
      Result : Times;

      begin -- To_HHMMSS
         if Decimal_Hour < 0.0 then
            Decimal_Hour := Decimal_Hour + C_R (Full_Day);
         end if; -- Decimal_Hour < 0.0
         if Decimal_Hour = C_R (Full_Day) then
            Decimal_Hour := 0.0;
         end if; -- = C_R (Full_Day)
      Result.Hour := Hour_Number (C_R'Truncation (Decimal_Hour));
      Decimal_Hour := (Decimal_Hour - C_R (Result.Hour)) * C_R (Sixty);
      Result.Minute := Minute_Number (C_R'Truncation (Decimal_Hour));
      Decimal_Hour := (Decimal_Hour - C_R (Result.Minute)) * C_R (Sixty);
      Result.Second := Minute_Number (C_R'Truncation (Decimal_Hour));
      return Result;
   end To_HHMMSS;

   function Julian_Day (Date : in Dates;
                        Time : in Times) return Julian_Days is

      Valid_Time : Ada.Calendar.Time :=
        Time_Of (Date.Year, Date.Month, Date.Day,
                 Time.Hour, Time.Minute, Time.Second);
      -- will raise exception if not valid Gregorian date
      This_Year : C_R := C_R (Date.Year);
      This_Month : C_R := C_R (Date.Month);
      This_Day : C_R;
      A, B : Integer;

   begin -- Julian_Day
      This_Day := C_R (Date.Day) + C_R (To_Hours (Time)) / C_R (Full_Day);
      if Date.Month <= 2 then
         This_Month := This_Month + 12.0;
         This_Year := This_Year - 1.0;
      end if; -- Date.Month <= 2
      A := Integer (C_R'Truncation (This_Year / 100.0));
      B := 2 - A + A / 4;
      return J_D (C_R (B) + C_R'Truncation (This_Year * 365.25) +
        C_R'Truncation ((This_Month + 1.0) * 30.6001) + This_Day + 1720994.5);
   end Julian_Day;

   function Julian_Day return Julian_Days is

      Now : constant Ada.Calendar.Time := Clock;
      Date : constant Dates := (Ada.Calendar.Formatting.Year (Now),
                                Ada.Calendar.Formatting.Month (Now),
                                Ada.Calendar.Formatting.Day (Now));
      Time : constant Times := (Ada.Calendar.Formatting.Hour (Now),
                                Ada.Calendar.Formatting.Minute (Now),
                                Ada.Calendar.Formatting.Second (Now));

   begin -- Julian_Day
      return Julian_Day (Date, Time);
   end Julian_Day;

   procedure From_Julian_Day (Julian_Day : in Julian_Days;
                              Date : out Dates;
                              Time : out Times) is

      This_Julian_Day : Julian_Days := Julian_Day + 0.5;
      Day_Int : Julian_Days;
      A, B, C, D, E, G : Integer;
      Decimal_Hour : Decimal_Hours;

   begin -- From_Julian_Day
      Day_Int := Julian_Days'Truncation (This_Julian_Day);
      Decimal_Hour := Decimal_Hours ((This_Julian_Day - Julian_Days(Day_Int))
                                     * 24.0);
      Time := To_HHMMSS (Decimal_Hour);
      A := Integer (Julian_Days'Truncation ((Day_Int - 1867216.25) / 36524.25));
      B := Integer (Day_Int) + 1 + A - (A / 4);
      C := B + 1524;
      D :=
        Integer (Julian_Days'Truncation ((Julian_Days (C) - 122.1) / 365.25));
      E := Integer (Julian_Days'Truncation (Julian_Days (D) * 365.25));
      G := Integer (Julian_Days'Truncation (Julian_Days (C - E) / 30.6001));
      Date.Day := C - E -
        Integer (Julian_Days'Truncation (Julian_Days (G) * 30.6001));
      if G > 13 then
         Date.Month := G - 13;
      else
         Date.Month := G - 1;
      end if; -- G > 13
      if Date.Month > 2 then
         Date.Year := D - 4716;
      else
         Date.Year := D - 4715;
      end if; -- Date.Month > 2;
   end From_Julian_Day;

   function From_Julian_Day (Julian_Day : in Julian_Days)
                             return Ada.Calendar.Time is

      Date : Dates;
      Time : Times;

   begin -- From_Julian_Day
      From_Julian_Day (Julian_Day, Date, Time);
      return Time_Of (Date.Year, Date.Month, Date.Day, Time.Hour, Time.Minute,
                      Time.Second);
   end From_Julian_Day;

   function Day_of_Year (Date : in Dates) return Year_Days is
     (Year_Days (J_D'Rounding (Julian_Day (Date, (0, 0, 0))
      - Julian_Day ((Date.Year, 1, 1), (0, 0, 0)))) + 1);
   -- Days counting 1 January of year as 1

   function UTC_To_GST (Date : in Dates;
                        Time : in Times) return Decimal_Hours is

      T, R, B, T0, Result : C_R;

   begin -- UTC_To_GST
      -- Days since 1 January of year, as an integer
      T := (C_R (Julian_Day ((Date.Year, 1, 1), (0, 0, 0))) - 2415020.0)
        / 36525.0;
      R := 6.6460656 + 2400.051262 * T + 0.00002581 * T ** 2;
      B := C_R (Full_Day) - R + C_R (Full_Day) * C_R (Date.Year - 1900);
      T0 := C_R (0.0657098 * C_R (Day_of_Year (Date) - 1) - B);
      Result :=  T0 + 1.002738 * C_R (To_Hours (Time));
      if Result < 0.0 then
         return D_H (Result + 24.0);
      else
         return D_H (Result);
      end if; -- Result < 0.0
   end UTC_To_GST;

   function UTC_To_GST return Decimal_Hours is

      Now : constant Ada.Calendar.Time := Clock;
      Date : constant Dates := (Ada.Calendar.Formatting.Year (Now),
                                Ada.Calendar.Formatting.Month (Now),
                                Ada.Calendar.Formatting.Day (Now));
      Time : constant Times := (Ada.Calendar.Formatting.Hour (Now),
                                Ada.Calendar.Formatting.Minute (Now),
                                Ada.Calendar.Formatting.Second (Now));

   begin -- UTC_To_GST
      return UTC_To_GST (Date, Time);
   end UTC_To_GST;

   function GST_To_UTC (Date : in Dates; Time : Times) return Decimal_Hours is

      T, R, B, T0, A : C_R;

   begin -- GST_To_UTC
      T := C_R (Julian_Day ((Date.Year, 1, 1), (0, 0, 0)) - 2415020.0)
        / 36525.0;
      R := 6.6460656 + 2400.051262 * T + 0.00002581 * T ** 2;
      B := C_R (Full_Day) - R + C_R (Full_Day) * C_R (Date.Year - 1900);
      T0 := 0.0657098 * C_R (Day_of_Year (Date) - 1) - B;
      if T0 > C_R (Full_Day) then
         T0 := T0 - C_R (Full_Day);
      elsif T0 < 0.0 then
         T0 := T0 + C_R (Full_Day);
      end if; -- T0 > C_R (Full_Day)
      A := C_R (To_Hours (Time)) - T0;
      if A < 0.0 then
         A := A + C_R (Full_Day);
      end if; -- A < 0.0
      return Decimal_Hours (0.997270 * A);
   end GST_To_UTC;

   function To_Time_Offset (Longitude : in Longitudes) return Time_Offsets is

      -- Note returns Celestial.Time_Offsets

      Per_Degree : constant Degrees := 24.0 / 360.0;

   begin
      if Longitude.Hemisphere = East then
         return Time_Offsets (Longitude.Angle * Per_Degree);
      else
         return Time_Offsets (-Longitude.Angle * Per_Degree);
      end if; -- Direction = West
   end To_Time_Offset;

   function To_Time_Offset (Longitude : in Longitudes)
                            return Ada.Calendar.Time_Zones.Time_Offset is

      -- Note returns Ada.Calendar.Time_Zones.Time_Offset

      Hour_Offset : constant Time_Offsets := To_Time_Offset (Longitude);

   begin -- To_Time_Offset
      return Time_Offset (C_R'Rounding (C_R (Hour_Offset) * C_R (Sixty)));
   end To_Time_Offset;

   procedure To_Greenwich (Offset : in Time_Offset;
                           Date : in out Dates;
                           Time : in out Times) is

      -- Uses Ada.Calendar.Formatting

      Ada_Time : Ada.Calendar.Time;
      Sub_Second : Second_Duration := 0.0;

   begin -- To_Greenwich
      Ada_Time := Time_Of (Date.Year, Date.Month, Date.Day,
                           Time.Hour, Time.Minute, Time.Second,
                           Sub_Second, Time_Zone => Offset);
      Split (Ada_Time, Date.Year, Date.Month, Date.Day,
             Time.Hour, Time.Minute, Time.Second,
             Sub_Second, Time_Zone => 0);
   end To_Greenwich;

   procedure To_Greenwich (Offset : in Time_Offsets;
                           Date : in out Dates;
                           Decimal_Hour : in out Decimal_Hours) is

      -- Uses Celestial.Time_Offsets which provides higher resolution than the
      -- Ada equivalemt which has one minute resolution or 15' of arc.

      One_Day : constant Day_Count := 1;

      Hour : C_R := C_R (Decimal_Hour - Offset);
      DMY : Ada.Calendar.Time :=
        Ada.Calendar.Formatting.Time_Of (Date.Year, Date.Month, Date.Day);

   begin -- To_Greenwich
      if Hour < 0.0 then
         Hour := Hour + C_R (Full_Day);
         DMY := DMY - One_Day;
      elsif Hour >= C_R (Full_Day) then
         Hour := Hour - C_R (Full_Day);
         DMY := DMY + One_Day;
      end if; -- Hour < 0.0
      Decimal_Hour := D_H (Hour);
      Date.Year := Ada.Calendar.Formatting.Year (DMY);
      Date.Month := Ada.Calendar.Formatting.Month (DMY);
      Date.Day := Ada.Calendar.Formatting.Day (DMY);
   end To_Greenwich;

   procedure To_Local (Offset : in Time_Offset;
                       Date : in out Dates;
                       Time : in out Times) is

      -- Uses Ada.Calendar.Formatting

      Ada_Time : Ada.Calendar.Time;
      Sub_Second : Second_Duration := 0.0;

   begin -- To_Local
      Ada_Time := Time_Of (Date.Year, Date.Month, Date.Day,
                           Time.Hour, Time.Minute, Time.Second,
                           Sub_Second, Time_Zone => 0);
      Split (Ada_Time, Date.Year, Date.Month, Date.Day,
             Time.Hour, Time.Minute, Time.Second,
             Sub_Second, Time_Zone => Offset);
   end To_Local;

   procedure To_Local (Offset : in Time_Offsets;
                       Date : in out Dates;
                       Decimal_Hour : in out Decimal_Hours) is

      One_Day : constant Day_Count := 1;

      Hour : C_R := C_R (Decimal_Hour + Offset);
      DMY : Ada.Calendar.Time :=
        Ada.Calendar.Formatting.Time_Of (Date.Year, Date.Month, Date.Day);

   begin -- To_Local
      if Hour < 0.0 then
         Hour := Hour + C_R (Full_Day);
         DMY := DMY - One_Day;
      elsif Hour >= C_R (Full_Day) then
         Hour := Hour - C_R (Full_Day);
         DMY := DMY + One_Day;
      end if; -- Hour < 0.0
      Decimal_Hour := D_H (Hour);
      Date.Year := Ada.Calendar.Formatting.Year (DMY);
      Date.Month := Ada.Calendar.Formatting.Month (DMY);
      Date.Day := Ada.Calendar.Formatting.Day (DMY);
   end To_Local;

end Celestial.Time;
