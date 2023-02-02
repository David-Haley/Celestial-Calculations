with Ada.Calendar.Time_Zones;
-- This packakage is intended to support conversions from time as defined by
-- Ada.Calendar to and from Julian Day Greenwich Siderial Time etc.
-- algorithms are based on Celestial Calculations by J L Lawrence.
-- All date calculations assumes Gregorian dates which will yeild a result in
-- the range of Ada Year_Number type. In general an exception will be raised if
-- a year outside this renge is used.

-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 02/02/2023

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

   Function To_Hours (Hour : in Hour_Number;
                      Minute : in Minute_Number;
                      Second : in Second_Number) return Decimal_Hours is
      (D_H (C_R (Hour) + (C_R (Minute) + C_R (Second) / C_R (Sixty))
          / C_R (Sixty)));

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
        Natural (C_R'Rounding (C_R (abs (Decimal_Hour)) * Hour_Seconds));

   begin -- To_HHMMSS
      -- Rounding could result in This_Time equal to 24:00:00
      if This_Hour >= Day_Seconds_N then
         This_Hour := Day_Seconds_N - 1;
      end if; -- This_Hour >= Day_Seconds_N
      Second := Second_Number (This_Hour mod Sixty);
      This_Hour := This_Hour / Sixty;
      Minute := Minute_Number (This_Hour mod Sixty);
      This_Hour := This_Hour / Sixty;
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
      This_Year : C_R := C_R (Year);
      This_Month : C_R := C_R (Month);
      This_Day : C_R;
      A, B : Integer;

   begin -- Julian_Day
      This_Day := C_R (Day) +
        C_R (To_Hours (Hour, Minute, Second)) / C_R (Full_Day);
      if Month <= 2 then
         This_Month := This_Month + 12.0;
         This_Year := This_Year - 1.0;
      end if; -- Month <= 2
      A := Integer (C_R'Truncation (This_Year / 100.0));
      B := 2 - A + A / 4;
      return J_D (C_R (B) + C_R'Truncation (This_Year * 365.25) +
        C_R'Truncation ((This_Month + 1.0) * 30.6001) + This_Day + 1720994.5);
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

   function Day_of_Year (Year : in Year_Number;
                         Month : in Month_Number;
                         Day : in Day_Number) return Year_Days is
     (Year_Days (J_D'Rounding (Julian_Day (Year, Month, Day, 0, 0, 0)
      - Julian_Day (Year, 1, 1, 0, 0, 0))) + 1);
   -- Days counting 1 January of year as 1

   function UTC_To_GST (Year : in Year_Number;
                        Month : in Month_Number;
                        Day : in Day_Number;
                        Hour : in Hour_Number;
                        Minute : in Minute_Number;
                        Second : in Second_Number) return Decimal_Hours is

      T, R, B, T0, Result : C_R;

   begin -- UTC_To_GST
      -- Days since 1 January of year, as an integer
      T := (C_R (Julian_Day (Year, 1, 1, 0, 0, 0)) - 2415020.0) / 36525.0;
      R := 6.6460656 + 2400.051262 * T + 0.00002581 * T ** 2;
      B := C_R (Full_Day) - R + C_R (Full_Day) * C_R (Year - 1900);
      T0 := C_R (0.0657098 * C_R (Day_of_Year (Year, Month, Day) - 1) - B);
      Result :=  T0 + 1.002738 * C_R (To_Hours (Hour, Minute, Second));
      if Result < 0.0 then
         return D_H (Result + 24.0);
      else
         return D_H (Result);
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

      T, R, B, T0, A : C_R;

   begin -- GST_To_UTC
      T := C_R (Julian_Day (Year, 1, 1, 0, 0, 0) - 2415020.0) / 36525.0;
      R := 6.6460656 + 2400.051262 * T + 0.00002581 * T ** 2;
      B := C_R (Full_Day) - R + C_R (Full_Day) * C_R (Year - 1900);
      T0 := 0.0657098 * C_R (Day_of_Year (Year, Month, Day) - 1) - B;
      if T0 > C_R (Full_Day) then
         T0 := T0 - C_R (Full_Day);
      elsif T0 < 0.0 then
         T0 := T0 + C_R (Full_Day);
      end if; -- T0 > C_R (Full_Day)
      A := C_R (To_Hours (Hour, Minute, Second)) - T0;
      if A < 0.0 then
         A := A + C_R (Full_Day);
      end if; -- A < 0.0
      return Decimal_Hours (0.997270 * A);
   end GST_To_UTC;

   function To_Time_Offset (Longitude : in Semis;
                            Direction : in Longitude_Directions)
                            return Time_Offsets is

      -- Note returns Celestial.Time_Offsets

      Per_Degree : constant Degrees := 24.0 / 360.0;

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

      -- Note returns Ada.Calendar.Time_Zones.Time_Offset

      Hour_Offset : Time_Offsets := To_Time_Offset (Longitude, Direction);

   begin -- To_Time_Offset
      return Time_Offset (C_R'Rounding (C_R (Hour_Offset) * C_R (Sixty)));
   end To_Time_Offset;

   procedure To_Greenwich (Offset : in Time_Offset;
                           Year : in out Year_Number;
                           Month : in out Month_Number;
                           Day : in out Day_Number;
                           Hour : in out Hour_Number;
                           Minute : in out Minute_Number;
                           Second : in out Second_Number) is

      -- Uses Ada.Calendar.Formatting

      Ada_Time : Ada.Calendar.Time;
      Sub_Second : Second_Duration := 0.0;

   begin -- To_Greenwich
      Ada_Time := Time_Of (Year, Month, Day, Hour, Minute, Second, Sub_Second,
                           Time_Zone => Offset);
      Split (Ada_Time, Year, Month, Day, Hour, Minute, Second, Sub_Second,
             Time_Zone => 0);
   end To_Greenwich;

   procedure To_Greenwich (Offset : in Time_Offsets;
                           Year : in out Year_Number;
                           Month : in out Month_Number;
                           Day : in out Day_Number;
                           Decimal_Hour : in out Decimal_Hours) is

      -- Uses Celestial.Time_Offsets which provides higher resolution than the
      -- Ada equivalemt which has one minute resolution or 15' of arc.

      One_Day : constant Day_Count := 1;

      Hour : C_R := C_R (Decimal_Hour - Offset);
      DMY : Ada.Calendar.Time :=
        Ada.Calendar.Formatting.Time_Of (Year, Month, Day);

   begin -- To_Greenwich
      if Hour < 0.0 then
         Hour := Hour + C_R (Full_Day);
         DMY := DMY - One_Day;
      elsif Hour >= C_R (Full_Day) then
         Hour := Hour - C_R (Full_Day);
         DMY := DMY + One_Day;
      end if; -- Hour < 0.0
      Decimal_Hour := D_H (Hour);
      Year := Ada.Calendar.Formatting.Year (DMY);
      Month := Ada.Calendar.Formatting.Month (DMY);
      Day := Ada.Calendar.Formatting.Day (DMY);
   end To_Greenwich;

   procedure To_Local (Offset : in Time_Offset;
                       Year : in out Year_Number;
                       Month : in out Month_Number;
                       Day : in out Day_Number;
                       Hour : in out Hour_Number;
                       Minute : in out Minute_Number;
                       Second : in out Second_Number) is

      -- Uses Ada.Calendar.Formatting

      Ada_Time : Ada.Calendar.Time;
      Sub_Second : Second_Duration := 0.0;

   begin -- To_Local
      Ada_Time := Time_Of (Year, Month, Day, Hour, Minute, Second, Sub_Second,
                           Time_Zone => 0);
      Split (Ada_Time, Year, Month, Day, Hour, Minute, Second, Sub_Second,
             Time_Zone => Offset);
   end To_Local;

   procedure To_Local (Offset : in Time_Offsets;
                       Year : in out Year_Number;
                       Month : in out Month_Number;
                       Day : in out Day_Number;
                       Decimal_Hour : in out Decimal_Hours) is

      One_Day : constant Day_Count := 1;

      Hour : C_R := C_R (Decimal_Hour + Offset);
      DMY : Ada.Calendar.Time :=
        Ada.Calendar.Formatting.Time_Of (Year, Month, Day);

   begin -- To_Local
      if Hour < 0.0 then
         Hour := Hour + C_R (Full_Day);
         DMY := DMY - One_Day;
      elsif Hour >= C_R (Full_Day) then
         Hour := Hour - C_R (Full_Day);
         DMY := DMY + One_Day;
      end if; -- Hour < 0.0
      Decimal_Hour := D_H (Hour);
      Year := Ada.Calendar.Formatting.Year (DMY);
      Month := Ada.Calendar.Formatting.Month (DMY);
      Day := Ada.Calendar.Formatting.Day (DMY);
   end To_Local;

end Celestial.Time;
