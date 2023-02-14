-- Test program for Celestial_Time

-- Author    : David Haley
-- Created   : 24/11/2019
-- Last Edit : 14/02/2023

-- 20230214 : Match changes in Dates and Times.
-- 20230204 : Declaration of Longitude Changed.
-- 20230202 : Additional tests for Ada.Calendar.Time_Zones.Time_Offset versions
-- of To_Greenwich and To_Local.
-- 20230131 : Test for Day_of_Year also test updated for To_Greenwich and
-- To_Local.

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
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
   package Offset_IO is new Ada.Text_IO.Integer_IO (Time_Offset);
   package Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Longitude_Directions);
   package Semis_IO is new Ada.Text_IO.Float_IO (Semis);

   procedure Test_To_Hours is

      Time : Times;

   begin -- Test_To_Hours
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Put_Line ("To_Hours:" & Decimal_Hours'Image(To_Hours (Time)));
   end Test_To_Hours;

   procedure Test_Julian_Day is

      Date : Dates;
      Time : times;

   begin -- Test_Julian_Day
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Put_Line ("Julian Day:" & Julian_Days'Image(Julian_Day (Date, Time)));
   end Test_Julian_Day;

   function Day_Of_Year return Year_Days is

      Date : Dates;

   begin -- Day_Of_Year
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      return Day_of_Year (Date);
   end Day_Of_Year;

   procedure To_DDMMYYYY is

      Julian_Day : Julian_Days;
      Date : Dates;
      Time : Times;

   begin -- To_DDMMYYYY
      Put ("Julian Day: ");
      Julian_IO.Get (Julian_Day);
      From_Julian_Day (Julian_Day, Date, Time);
      Day_IO.Put (Date.Day, 0);
      Put ('/');
      Month_IO.Put (Date.Month, 0);
      Put ('/');
      Year_IO.Put (Date.Year, 0);
      Put ("   ");
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
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

      Date : Dates;
      Decimal_Hour : Decimal_Hours;
      Time : Times;

   begin -- Test_UTC_To_GST
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Decimal_Hour := UTC_To_GST (Date, Time);
      Put ("GST:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      Time := To_HHMMSS (Decimal_Hour);
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      New_Line;
   end Test_UTC_To_GST;

   procedure Current_UTC_To_GST is

      Decimal_Hour : Decimal_Hours;
      Time : Times;

   begin -- Current_UTC_To_GST
      Decimal_Hour := UTC_To_GST;
      Put ("GST:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      Time := To_HHMMSS (Decimal_Hour);
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      New_Line;
   end Current_UTC_To_GST;

   procedure Test_GST_To_UTC is
      Date : Dates;
      Time : Times;
      Decimal_Hour : Decimal_Hours;

   begin -- Test_GST_To_UTC
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Decimal_Hour := GST_To_UTC (Date, Time);
      Put ("UTC:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      Time := To_HHMMSS (Decimal_Hour);
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      New_Line;
   end Test_GST_To_UTC;

   procedure Local_To_Greenwich_Civil is

      Date : Dates;
      Time : Times;
      Offset : Time_Offset;

   begin -- Local_To_Greenwich_Civil
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Put ("Offset (Minutes): ");
      Offset_IO.Get (Offset);
      To_Greenwich (Offset, Date, Time);
      Put ("Greenwich: ");
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      Put (" on ");
      Day_IO.Put (Date.Day, 0);
      Put ('/');
      Month_IO.Put (Date.Month, 0);
      Put ('/');
      Year_IO.Put (Date.Year, 0);
      New_Line;
   end Local_To_Greenwich_Civil;

   procedure Local_To_Greenwich is

      Date : Dates;
      Decimal_Hour : Decimal_Hours;
      Longitude : Longitudes;
      Offset : Time_Offsets;
      Time : Times;

   begin -- Local_To_Greenwich
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Decimal_Hour := To_Hours (Time);
      Put ("Longitude.Angle: ");
      Semis_IO.Get (Longitude.Angle);
      Put ("Longitude.Hemisphere: ");
      Direction_IO.Get (Longitude.Hemisphere);
      Offset := To_Time_Offset (Longitude);
      Put ("Offset:" & Decimal_Hours'Image (Offset) & "  ");
      To_Greenwich (Offset, Date, Decimal_Hour);
      Put ("Greenwich:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      Time := To_HHMMSS (Decimal_Hour);
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      Put (" on ");
      Day_IO.Put (Date.Day, 0);
      Put ('/');
      Month_IO.Put (Date.Month, 0);
      Put ('/');
      Year_IO.Put (Date.Year, 0);
      New_Line;
   end Local_To_Greenwich;

   procedure Greenwich_To_Local_Civil is

      Date : Dates;
      Offset : Time_Offset;
      Time : Times;

   begin -- Greenwich_To_Local_Civil
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Put ("Offset (Minures): ");
      Offset_IO.Get (Offset);
      To_Local (Offset, Date, Time);
      Put ("Local:");
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      Put (" on ");
      Day_IO.Put (Date.Day, 0);
      Put ('/');
      Month_IO.Put (Date.Month, 0);
      Put ('/');
      Year_IO.Put (Date.Year, 0);
      New_Line;
   end Greenwich_To_Local_Civil;

   procedure Greenwich_To_Local is

      Date : Dates;
      Decimal_Hour : Decimal_Hours;
      Longitude : Longitudes;
      Offset : Time_Offsets;
      Time : Times;

   begin -- Greenwich_To_Local
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put ("Hour: ");
      Hour_IO.Get (Time.Hour);
      Put ("Minute: ");
      Minute_IO.Get (Time.Minute);
      Put ("Second: ");
      Second_IO.Get (Time.Second);
      Decimal_Hour := To_Hours (Time);
      Put ("Longitude.Amgle: ");
      Semis_IO.Get (Longitude.Angle);
      Put ("Longitude.Hemishpere: ");
      Direction_IO.Get (Longitude.Hemisphere);
      Offset := To_Time_Offset (Longitude);
      Put ("Offset:" & Decimal_Hours'Image (Offset) & "  ");
      To_Local (Offset, Date, Decimal_Hour);
      Put ("Local:" & Decimal_Hours'Image (Decimal_Hour) & " or ");
      Time := To_HHMMSS (Decimal_Hour);
      Hour_IO.Put (Time.Hour, 0);
      Put (':');
      Minute_IO.Put (Time.Minute, 0);
      Put (':');
      Second_IO.Put (Time.Second, 0);
      Put (" on ");
      Day_IO.Put (Date.Day, 0);
      Put ('/');
      Month_IO.Put (Date.Month, 0);
      Put ('/');
      Year_IO.Put (Date.Year, 0);
      New_Line;
   end Greenwich_To_Local;

begin -- Test_Time
   loop -- Perform one test
      Put_Line ("A HH:MM:SS to HH.hhhh");
      Put_Line ("B Current UTC to HH.hhhh");
      Put_Line ("C DD/MM/YYYY HH:MM:SS to Julian Days");
      Put_Line ("D Current UTC to Julian Days");
      Put_Line ("E Day of Year, from 1 January of year");
      Put_Line ("F Julian Day to DD/MM/YYYY HH:MM:SS");
      Put_Line ("G Julian Day to Ada.Calendar.Time");
      Put_Line ("H UTC to GST");
      Put_Line ("I Current_UTC to GST");
      Put_Line ("J GST to UTC");
      Put_Line ("K Local Time to Greenwich Time (offest MMMM)");
      Put_Line ("L Local Time to Greenwich Time (longitude)");
      Put_Line ("M Greenwich Time to Local Time (offest MMMM)");
      Put_Line ("N Greenwich Time to Local Time (longitude)");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case To_Upper (Test) is
         when 'A' =>
            Test_To_Hours;
         when 'B'  =>
            Put_Line ("To_Hours:" & Decimal_Hours'Image (To_Hours));
         when 'C' =>
            Test_Julian_Day;
         when 'D' =>
            Put_Line ("Julian Day:" & Julian_Days'Image (Julian_Day));
         when 'E' =>
            Put_Line ("Day of Year:" & Day_of_Year'Img);
         when 'F' =>
            To_DDMMYYYY;
         when 'G' =>
            To_Calandar;
         when 'H' =>
            Test_UTC_To_GST;
         when 'I' =>
            Current_UTC_To_GST;
         when 'J' =>
            Test_GST_To_UTC;
         when 'K' =>
            Local_To_Greenwich_Civil;
         when 'L' =>
            Local_To_Greenwich;
         when 'M' =>
            Greenwich_To_Local_Civil;
         when 'N' =>
            Greenwich_To_Local;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Time;
