-- Test program for Celestial.Coordinates
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 04/02/2023

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Celestial; use Celestial;
with Celestial.Coordinates; use Celestial.Coordinates;

procedure Test_Coordinates is

   Test : Character;

   package Radian_IO is new Ada.Text_IO.Float_IO (Radians);
   package Time_Offset_IO is new Ada.Text_IO.Float_IO (Celestial.Time_Offsets);
   package Degree_IO is new Ada.Text_IO.Float_IO (Degrees);

   procedure To_Latitude_R is

      Angle : Radians;
      Latitude : Latitudes;

   begin -- To_Latitude_R
      Put ("Angle: ");
      Radian_IO.Get (Angle);
      Latitude := To_Latitude (Angle);
      Put_Line ("Latitude:" & Latitude.Angle'Img & " " &
                  Latitude.Hemisphere'Img);
   end To_Latitude_R;

   procedure To_Latitude_D is

      Angle : Degrees;
      Latitude : Latitudes;

   begin -- To_Latitude_D
      Put ("Angle: ");
      Degree_IO.Get (Angle);
      Latitude := To_Latitude (Angle);
      Put_Line ("Latitude:" & Latitude.Angle'Img & " " &
                  Latitude.Hemisphere'Img);
   end To_Latitude_D;

   procedure To_Longitude_R is

      Angle : Radians;
      Longitude : Longitudes;

   begin -- To_Longitude_R
      Put ("Angle: ");
      Radian_IO.Get (Angle);
      Longitude := To_Longitude (Angle);
      Put_Line ("Longitude:" & Longitude.Angle'Img & " " &
                  Longitude.Hemisphere'Img);
   end To_Longitude_R;

   procedure To_Longitude_D is

      Angle : Degrees;
      Longitude : Longitudes;

   begin -- To_Longitude_D
      Put ("Angle: ");
      Degree_IO.Get (Angle);
      Longitude := To_Longitude (Angle);
      Put_Line ("Longitude:" & Longitude.Angle'Img & " " &
                  Longitude.Hemisphere'Img);
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

begin --  Test_Coordinates
   loop -- Perform one test
      Put_Line ("A To Latitude (Radians)");
      Put_Line ("B To Latitude (Degrees)");
      Put_Line ("C To Longitude (Radians)");
      Put_Line ("D To Longitude (Degrees)");
      Put_Line ("E To Longitude (Time_Offset)");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case To_Upper (Test) is
         when 'A' =>
            To_Latitude_R;
         when 'B' =>
            To_Latitude_D;
         when 'C' =>
            To_Longitude_R;
         when 'D' =>
             To_Longitude_D;
         when 'E' =>
            To_Longitude_To;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Coordinates;
