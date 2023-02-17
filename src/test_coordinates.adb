-- Test program for Celestial.Coordinates
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 17/02/2023

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

   package Radian_IO is new Ada.Text_IO.Float_IO (Radians);
   package Time_Offset_IO is new Ada.Text_IO.Float_IO (Celestial.Time_Offsets);
   package Degree_IO is new Ada.Text_IO.Float_IO (Degrees);
   package Latitude_Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Latitude_Directions);

   procedure To_DDDMMSS is

      Angle : Degrees;
      DDDMMSS : DDDMMSSs;

   begin -- To_DDDMMSS
      Put ("Angle: ");
      Degree_IO.Get (Angle);
      DDDMMSS := To_DDDMMSS (Angle);
      Put_Line (DDDMMSS.Degree'Img & Ada.Characters.Latin_1.Degree_Sign &
                  DDDMMSS.Minute'Img & "'" & DDDMMSS.Second'Img & """"
                & " Difference: " &
                  Degrees'Image (Angle - To_Degrees (DDDMMSS)));
   end To_DDDMMSS;

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
      Put_Line ("A Degrees to DDMMSS");
      Put_Line ("B To Latitude (Radians)");
      Put_Line ("C To Latitude (Degrees)");
      Put_Line ("D To Longitude (Radians)");
      Put_Line ("E To Longitude (Degrees)");
      Put_Line ("F To Longitude (Time_Offset)");
      Put_Line ("G To Equatorial (Altitude, Azimuth, Latitude)");
      Put_Line ("H Obliquity of the Ecliptic (Date)");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case To_Upper (Test) is
         when 'A' =>
            To_DDDMMSS;
         WHEN 'B' =>
            To_Latitude_R;
         when 'C' =>
            To_Latitude_D;
         when 'D' =>
            To_Longitude_R;
         when 'E' =>
             To_Longitude_D;
         when 'F' =>
            To_Longitude_To;
         when 'G' =>
            To_Equatorial_AAL;
         when 'H' =>
            Obliquity_Ecliptic;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Coordinates;
