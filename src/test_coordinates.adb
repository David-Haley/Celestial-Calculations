-- Test program for Celestial.Coordinates
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 17/02/2023

-- 20230218 : To_Altitude and To_Azimuth added.
-- 20230217 : Obliquity of the Ecliptic added.
-- 20230206 : test Degrees to DDMMSS
-- 20230205 : To_Angle conversion errors reported, tests conversion from Horison
-- to Equatorial Coordinates.

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Celestial; use Celestial;
with Celestial.Time; use Celestial.Time;
with Celestial.Coordinates; use Celestial.Coordinates;

procedure Test_Coordinates is

   package Year_IO is new Ada.Text_IO.Integer_IO (Year_Number);
   package Month_IO is new Ada.Text_IO.Integer_IO (Month_Number);
   package Day_IO is new Ada.Text_IO.Integer_IO (Day_Number);
   package Hour_IO is new Ada.Text_IO.Integer_IO (Hour_Number);
   package Minute_IO is new Ada.Text_IO.Integer_IO (Minute_Number);
   package Second_IO is new Ada.Text_IO.Integer_IO (Second_Number);
   package Radian_IO is new Ada.Text_IO.Float_IO (Radians);
   package Time_Offset_IO is new Ada.Text_IO.Float_IO (Celestial.Time_Offsets);
   package Degree_IO is new Ada.Text_IO.Float_IO (Degrees);
   package DDD_IO is new Ada.Text_IO.Integer_IO (Degrees_N);
   package MM_IO is new Ada.Text_IO.Integer_IO (Minutes_N);
   package SS_IO is new Ada.Text_IO.Integer_IO (Seconds_N);
   package Latitude_Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Latitude_Directions);

   procedure To_DDDMMSS is

      Angle : Degrees;
      DDDMMSS : DDDMMSSs;

   begin -- To_DDDMMSS
      Put ("Angle: ");
      Degree_IO.Get (Angle);
      DDDMMSS := To_DDDMMSS (Angle);
      Put_Line ("Angle:" & DDDMMSS.Degree'Img &
                  Ada.Characters.Latin_1.Degree_Sign &
                  DDDMMSS.Minute'Img & "'" & DDDMMSS.Second'Img & """");
   end To_DDDMMSS;

   procedure To_Degrees is

      Angle : DDDMMSSs;

   begin -- To_Degrees
      Put ("Angle in DDD MM SS format: ");
      DDD_IO.Get (Angle.Degree);
      MM_IO.Get (Angle.Minute);
      SS_IO.Get (Angle.Second);
      Put_Line ("Angle:" & To_Degrees (Angle)'Img);
   end To_Degrees;

   procedure To_Latitude_R is

      Angle : Radians;
      Latitude : Latitudes;

   begin -- To_Latitude_R
      Put ("Angle: ");
      Radian_IO.Get (Angle);
      Latitude := To_Latitude (Angle);
      Put_Line ("Latitude:" & Latitude.Angle'Img & " " &
                  Latitude.Hemisphere'Img & " Error: " &
                  Radians'Image (Angle - To_Angle (Latitude)));
   end To_Latitude_R;

   procedure To_Latitude_D is

      Angle : Degrees;
      Latitude : Latitudes;

   begin -- To_Latitude_D
      Put ("Angle: ");
      Degree_IO.Get (Angle);
      Latitude := To_Latitude (Angle);
      Put_Line ("Latitude:" & Latitude.Angle'Img & " " &
                  Latitude.Hemisphere'Img & " Error: " &
                  Degrees'Image (Angle - To_Angle (Latitude)));
   end To_Latitude_D;

   procedure To_Longitude_R is

      Angle : Radians;
      Longitude : Longitudes;

   begin -- To_Longitude_R
      Put ("Angle: ");
      Radian_IO.Get (Angle);
      Longitude := To_Longitude (Angle);
      Put_Line ("Longitude:" & Longitude.Angle'Img & " " &
                  Longitude.Hemisphere'Img & " Error: " &
                  Radians'Image (Angle - To_Angle (Longitude)));
   end To_Longitude_R;

   procedure To_Longitude_D is

      Angle : Degrees;
      Longitude : Longitudes;

   begin -- To_Longitude_D
      Put ("Angle: ");
      Degree_IO.Get (Angle);
      Longitude := To_Longitude (Angle);
      Put_Line ("Longitude:" & Longitude.Angle'Img & " " &
                  Longitude.Hemisphere'Img & " Error: " &
                  Degrees'Image (Angle - To_Angle (Longitude)));
   end To_Longitude_D;

   procedure To_Longitude_To is

      Time_Offset : Time_Offsets;
      Longitude : Longitudes;

   begin -- To_Longitude_To
      Put ("Time_Offset: ");
      Time_Offset_IO.Get (Time_Offset);
      Longitude := To_Longitude (Time_Offset);
      Put_Line ("Longitude:" & Longitude.Angle'Img & " " &
                  Longitude.Hemisphere'Img);
   end To_Longitude_To;

   procedure To_Equatorial_AAL is

      Altitude : Altitudes;
      Azimuth : Azimuths;
      Latitude : Latitudes;

   begin -- To_Equatorial_AAL
      Put ("Altitude: ");
      Degree_IO.Get (Altitude);
      Put ("Azimuth: ");
      Degree_IO.Get (Azimuth);
      Put ("Latitude.Hemisphere: ");
      Latitude_Direction_IO.Get (Latitude.Hemisphere);
      Put ("Latitude.Angle: ");
      Degree_IO.Get (Latitude.Angle);
      Put_Line ("Declination:" &
                  To_Declination (Altitude, Azimuth, Latitude)'Img &
                  " Hour angle:" &
                  To_Hour_Angle (Altitude, Azimuth, Latitude)'Img);
   end To_Equatorial_AAL;

   procedure To_Horizon is

      Declination : Declinations;
      Hour_Angle : Right_Ascensions;
      H_A : Times;
      Latitude : Latitudes;

   begin -- To_Horizon
      Put ("Declination: ");
      Degree_IO.Get (Declination);
      Put ("Hour Angle in HH MM SS format: ");
      Hour_IO.Get (H_A.Hour);
      Minute_IO.Get (H_A.Minute);
      Second_IO.Get (H_A.Second);
      Hour_Angle := To_Hours (H_A);
      Put ("Latitude.Hemisphere: ");
      Latitude_Direction_IO.Get (Latitude.Hemisphere);
      Put ("Latitude.Angle: ");
      Degree_IO.Get (Latitude.Angle);
      Put_Line ("Altitude:" &
                  To_Altitude (Declination, Hour_Angle, Latitude)'Img &
                  " Azimuth:" &
                  To_Azimuth (Declination, Hour_Angle, Latitude)'Img);
   end To_Horizon;

   procedure Obliquity_Ecliptic is

      Date : Dates;

   begin -- Obliquity_Ecliptic
      Put ("Year: ");
      Year_IO.Get (Date.Year);
      Put ("Month: ");
      Month_IO.Get (Date.Month);
      Put ("Day: ");
      Day_IO.Get (Date.Day);
      Put_Line ("Obliquity of the Ecliptic:" & Obliquity_Ecliptic (Date)'Img);
   end Obliquity_Ecliptic;

   Test : Character;

begin --  Test_Coordinates
   loop -- Perform one test
      Put_Line ("A Degrees to DDDMMSS");
      Put_Line ("B DDDMMSS to Degrees");
      Put_Line ("C To Latitude (Radians)");
      Put_Line ("D To Latitude (Degrees)");
      Put_Line ("E To Longitude (Radians)");
      Put_Line ("F To Longitude (Degrees)");
      Put_Line ("G To Longitude (Time_Offset)");
      Put_Line ("H To Equatorial (Altitude, Azimuth, Latitude)");
      Put_Line ("I To Horizon (Declination, Hour Angle, Latitude)");
      Put_Line ("J Obliquity of the Ecliptic (Date)");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case To_Upper (Test) is
         when 'A' =>
            To_DDDMMSS;
         when 'B' =>
            To_Degrees;
         when 'C' =>
            To_Latitude_R;
         when 'D' =>
            To_Latitude_D;
         when 'E' =>
            To_Longitude_R;
         when 'F' =>
             To_Longitude_D;
         when 'G' =>
            To_Longitude_To;
         when 'H' =>
            To_Equatorial_AAL;
         when 'I' =>
            To_Horizon;
         when 'J' =>
            Obliquity_Ecliptic;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Coordinates;
