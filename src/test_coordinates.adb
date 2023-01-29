-- Test program for Celestial.Coordinates
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 26/03/2020

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO; use Ada.Text_IO;
with Celestial; use Celestial;
with Celestial.Coordinates; use Celestial.Coordinates;

procedure Test_Coordinates is

   Test : Character;

   package Radian_IO is new Ada.Text_IO.Float_IO (Radians);
   package Time_Offset_IO is new Ada.Text_IO.Float_IO (Celestial.Time_Offsets);
   package Degree_IO is new Ada.Text_IO.Float_IO (Degrees);

   procedure Degrees_to_Radians is

      Angle : Degrees;

   begin -- Degrees_to_Radians
      put ("Angle (Degrees): ");
      Degree_IO.Get (Angle);
      Put_Line ("To_Radians:" & Radians'Image (To_Radians (Angle)));
   end Degrees_to_Radians;

   procedure Radians_to_Degrees is

      Angle : Radians;

   begin -- Radians_to_Degrees
      put ("Angle (Radians): ");
      Radian_IO.Get (Angle);
      Put_Line ("To_Degrees:" & Degrees'Image (To_Degrees (Angle)));
   end Radians_to_Degrees;

   procedure Test_To_Longitud is

      Time_Offset : Time_Offsets;

   begin -- Test_To_Longitud
      Put ("Time_Offset: ");
      Time_Offset_IO.Get (Time_Offset);
      Put_Line ("To_Longitude:" &
                  Degrees'Image(To_Longitude (Time_Offset)));
   end Test_To_Longitud;

begin --  Test_Coordinates
   loop -- Perform one test
      Put_Line ("A Degrees to Radians");
      Put_Line ("B Radians to Degrees");
      Put_Line ("C Time_Offset to Degrees");
      Put_Line ("0 Exit tests");
      Put ("Test: ");
      Get (Test);
      exit when Test = '0';
      case Test is
         when 'a' | 'A' =>
            Degrees_to_Radians;
         when 'b' | 'B' =>
            Radians_to_Degrees;
         when 'c' | 'C' =>
            Test_To_Longitud;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Coordinates;
