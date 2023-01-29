-- Test program for Celestial_Time
-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 27/11/2019

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
with Celestial; use Celestial;
with Celestial.Time; use Celestial.Time;

procedure Test_Time is

   Test : Character;

   package Year_IO is new Ada.Text_IO.Integer_IO (Year_Number);
   package Month_IO is new Ada.Text_IO.Integer_IO (Month_Number);
   package Day_IO is new Ada.Text_IO.Integer_IO (Day_Number);
   package Hour_IO is new Ada.Text_IO.Integer_IO (Hour_Number);
   package Minute_IO is new Ada.Text_IO.Integer_IO (Minute_Number);
   package Second_IO is new Ada.Text_IO.Integer_IO (Second_Number);
   package Julian_IO is new Ada.Text_IO.Float_IO (Julian_Days);
   package Decimal_IO is new Ada.Text_IO.Float_IO (Decimal_Hours);
   package Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Longitude_Directions);
   package Semis_IO is new Ada.Text_IO.Float_IO (Semis);

   procedure Test_To_Hours is

      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- Test_To_Hours
      Put ("Hour: ");
      Hour_IO.Get (Hour);
      Put ("Minute: ");
      Minute_IO.Get (Minute);
      Put ("Second: ");
      Second_IO.Get (Second);
      Put_Line ("To_Hours:" &
                  Decimal_Hours'Image(To_Hours (Hour, Minute, Second)));
   end Test_To_Hours;

   procedure Test_Julian_Day is

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- Test_Julian_Day
      Put ("Year: ");
      Year_IO.Get (Year);
      Put ("Month: ");
      Month_IO.Get (Month);
      Put ("Day: ");
      Day_IO.Get (Day);
      Put ("Hour: ");
      Hour_IO.Get (Hour);
      Put ("Minute: ");
      Minute_IO.Get (Minute);
      Put ("Second: ");
      Second_IO.Get (Second);
      Put_Line ("Julian Day:" &
                  Julian_Days'Image(Julian_Day (Year, Month, Day,
                    Hour, Minute, Second)));
   end Test_Julian_Day;

   procedure To_DDMMYYYY is

      Julian_Day : Julian_Days;
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- To_DDMMYYYY
      Put ("Julian Day: ");
      Julian_IO.Get (Julian_Day);
      From_Julian_Day (Julian_Day, Year, Month, Day, Hour, Minute, Second);
      Day_IO.Put (Day, 0);
      Put ('/');
      Month_IO.Put (Month, 0);
      Put ('/');
      Year_IO.Put (Year, 0);
      Put ("   ");
      Hour_IO.Put (Hour, 0);
      Put (':');
      Minute_IO.Put (Minute, 0);
      Put (':');
      Second_IO.Put (Second, 0);
      New_Line;
   end To_DDMMYYYY;

   procedure To_Calandar is

      Julian_Day : Julian_Days;

   begin -- To_Calandar
      Put ("Julian Day: ");
      Julian_IO.Get (Julian_Day);
      Put_Line (Image (From_Julian_Day (Julian_Day)));
   end To_Calandar;

   procedure Test_UTC_To_GST is

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Decimal_Hour : Decimal_Hours;

   begin -- Test_UTC_To_GST
      Put ("Year: ");
      Year_IO.Get (Year);
      Put ("Month: ");
      Month_IO.Get (Month);
      Put ("Day: ");
      Day_IO.Get (Day);
      Put ("Hour: ");
      Hour_IO.Get (Hour);
      Put ("Minute: ");
      Minute_IO.Get (Minute);
      Put ("Second: ");
      Second_IO.Get (Second);
      Decimal_Hour := UTC_To_GST (Year, Month, Day, Hour, Minute, Second);
      Put ("GST:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      To_HHMMSS (Decimal_Hour, Hour, Minute, Second);
      Hour_IO.Put (Hour, 0);
      Put (':');
      Minute_IO.Put (Minute, 0);
      Put (':');
      Second_IO.Put (Second, 0);
      New_Line;
   end Test_UTC_To_GST;

   procedure Current_UTC_To_GST is
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Decimal_Hour : Decimal_Hours;

   begin -- Current_UTC_To_GST
      Decimal_Hour := UTC_To_GST;
      Put ("GST:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      To_HHMMSS (Decimal_Hour, Hour, Minute, Second);
      Hour_IO.Put (Hour, 0);
      Put (':');
      Minute_IO.Put (Minute, 0);
      Put (':');
      Second_IO.Put (Second, 0);
      New_Line;
   end Current_UTC_To_GST;

   procedure Test_GST_To_UTC is

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Decimal_Hour : Decimal_Hours;

   begin -- Test_GST_To_UTC
      Put ("Year: ");
      Year_IO.Get (Year);
      Put ("Month: ");
      Month_IO.Get (Month);
      Put ("Day: ");
      Day_IO.Get (Day);
      Put ("Hour: ");
      Hour_IO.Get (Hour);
      Put ("Minute: ");
      Minute_IO.Get (Minute);
      Put ("Second: ");
      Second_IO.Get (Second);
      Decimal_Hour := GST_To_UTC (Year, Month, Day, Hour, Minute, Second);
      Put ("UTC:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      To_HHMMSS (Decimal_Hour, Hour, Minute, Second);
      Hour_IO.Put (Hour, 0);
      Put (':');
      Minute_IO.Put (Minute, 0);
      Put (':');
      Second_IO.Put (Second, 0);
      New_Line;
   end Test_GST_To_UTC;

   procedure Local_To_Greenwich is

      Decimal_Hour : Decimal_Hours;
      Longitude : Semis;
      Direction : Longitude_Directions;
      Offset : Time_Offsets;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- Local_To_Greenwich
      Put ("Decimal Time: ");
      Decimal_IO.Get (Decimal_Hour);
      Put ("Longitude: ");
      Semis_IO.Get (Longitude);
      Put ("Direction: ");
      Direction_IO.Get (Direction);
      Offset := To_Time_Offset (Longitude, Direction);
      Put ("Offset:" & Decimal_Hours'Image (Offset) & "  ");
      Decimal_Hour := To_Greenwich (Decimal_Hour, Offset);
      Put ("Greenwich:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      To_HHMMSS (Decimal_Hour, Hour, Minute, Second);
      Hour_IO.Put (Hour, 0);
      Put (':');
      Minute_IO.Put (Minute, 0);
      Put (':');
      Second_IO.Put (Second, 0);
      New_Line;
   end Local_To_Greenwich;

   procedure Greenwich_To_Local is

      Decimal_Hour : Decimal_Hours;
      Longitude : Semis;
      Direction : Longitude_Directions;
      Offset : Time_Offsets;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

   begin -- Greenwich_To_Local
      Put ("Decimal Time: ");
      Decimal_IO.Get (Decimal_Hour);
      Put ("Longitude: ");
      Semis_IO.Get (Longitude);
      Put ("Direction: ");
      Direction_IO.Get (Direction);
      Offset := To_Time_Offset (Longitude, Direction);
      Put ("Offset:" & Decimal_Hours'Image (Offset) & "  ");
      Decimal_Hour := To_Local (Decimal_Hour, Offset);
      Put ("Local:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      To_HHMMSS (Decimal_Hour, Hour, Minute, Second);
      Hour_IO.Put (Hour, 0);
      Put (':');
      Minute_IO.Put (Minute, 0);
      Put (':');
      Second_IO.Put (Second, 0);
      New_Line;
   end Greenwich_To_Local;

begin -- Test_Time
   loop -- Perform one test
      Put_Line ("A HH:MM:SS to HH.hhhh");
      Put_Line ("B Current UTC to HH.hhhh");
      Put_Line ("C DD/MM/YYYY HH:MM:SS to Julian Days");
      Put_Line ("D Current UTC to Julian Days");
      Put_Line ("E Julian Day to DD/MM/YYYY HH:MM:SS");
      Put_Line ("F Julian Day to Ada.Calendar.Time");
      Put_Line ("G UTC to GST");
      Put_Line ("H Current_UTC to GST");
      Put_Line ("I GST to UTC");
      Put_Line ("J Local Time to Greenwich Time");
      Put_Line ("K Greenwich Time to Local Time");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case Test is
         when 'a' | 'A' =>
            Test_To_Hours;
         when 'b' | 'B' =>
            Put_Line ("To_Hours:" & Decimal_Hours'Image (To_Hours));
         when 'c' | 'C' =>
            Test_Julian_Day;
         when 'd' | 'D' =>
            Put_Line ("Julian Day:" & Julian_Days'Image (Julian_Day));
         when 'e' | 'E' =>
            To_DDMMYYYY;
         when 'f' | 'F' =>
            To_Calandar;
         when 'g' | 'G' =>
            Test_UTC_To_GST;
         when 'h' | 'H' =>
            Current_UTC_To_GST;
         when 'i' | 'I' =>
            Test_GST_To_UTC;
         when 'j' | 'J' =>
            Local_To_Greenwich;
         when 'k' | 'K' =>
            Greenwich_To_Local;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Time;
