-- Test program for Celestial.Coordinates
-- Author    : David Haley
-- Created   : 25/03/2020
-- Last Edit : 28/02/2023

-- 20230228 : Coordinate components made types not subtypes;
-- 20230227 : Tests for Precession_Correction and end of chapter exercises
-- added.
-- 20230225 : Tests for conversions between Equatorial and Ecliptic corrdinates.
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

   package Altitude_IO is new Ada.Text_IO.Float_IO (Altitudes);
   package Azimuth_IO is new Ada.Text_IO.Float_IO (Azimuths);
   package Day_IO is new Ada.Text_IO.Integer_IO (Day_Number);
   package DDD_IO is new Ada.Text_IO.Integer_IO (Degrees_N);
   package Declination_IO is new Ada.Text_IO.Float_IO (Declinations);
   package Degree_IO is new Ada.Text_IO.Float_IO (Degrees);
   package Ecliptic_Latitude_IO is new
     Ada.Text_IO.Float_IO (Ecliptic_Latitudes);
   package Ecliptic_Longitude_IO is new
     Ada.Text_IO.Float_IO (Ecliptic_Longitudes);
   package Hour_IO is new Ada.Text_IO.Integer_IO (Hour_Number);
   package Latitude_Direction_IO is new
     Ada.Text_IO.Enumeration_IO (Latitude_Directions);
   package Minute_IO is new Ada.Text_IO.Integer_IO (Minute_Number);
   package Month_IO is new Ada.Text_IO.Integer_IO (Month_Number);
   package MM_IO is new Ada.Text_IO.Integer_IO (Minutes_N);
   package Radian_IO is new Ada.Text_IO.Float_IO (Radians);
   package Second_IO is new Ada.Text_IO.Integer_IO (Second_Number);
   package SS_IO is new Ada.Text_IO.Integer_IO (Seconds_N);
   package Time_Offset_IO is new Ada.Text_IO.Float_IO (Celestial.Time_Offsets);
   package Year_IO is new Ada.Text_IO.Integer_IO (Year_Number);

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
      Altitude_IO.Get (Altitude);
      Put ("Azimuth: ");
      Azimuth_IO.Get (Azimuth);
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
      Declination_IO.Get (Declination);
      Put ("Hour Angle in HH MM SS format: ");
      Hour_IO.Get (H_A.Hour);
      Minute_IO.Get (H_A.Minute);
      Second_IO.Get (H_A.Second);
      Hour_Angle := Right_Ascensions (To_Hours (H_A));
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
      Put ("Date in DD MM YYYY format: ");
      Day_IO.Get (Date.Day);
      Month_IO.Get (Date.Month);
      Year_IO.Get (Date.Year);
      Put_Line ("Obliquity of the Ecliptic:" & Obliquity_Ecliptic (Date)'Img);
   end Obliquity_Ecliptic;

   procedure To_Equatorial_ELaELoD is

      Ecliptic_Latitude : Ecliptic_Latitudes;
      Ecliptic_Longitude : Ecliptic_Longitudes;
      Date : Dates;
      Declination : Declinations;
      Right_Ascension : Right_Ascensions;

   begin -- To_Equatorial_ELaELoD
      Put ("Ecliptic Latitude: ");
      Ecliptic_Latitude_IO.Get (Ecliptic_Latitude);
      Put ("Ecliptic Longitude: ");
      Ecliptic_Longitude_IO.Get (Ecliptic_Longitude);
      Put ("Date in DD MM YYYY format: ");
      Day_IO.Get (Date.Day);
      Month_IO.Get (Date.Month);
      Year_IO.Get (Date.Year);
      Declination := To_Declination (Ecliptic_Latitude, Ecliptic_Longitude,
                                     Date);
      Right_Ascension := To_Right_Ascension (Ecliptic_Latitude,
                                             Ecliptic_Longitude, Date);
      Put_Line ("Declination:" & Declination'Img & " Right Ascension:" &
                  Right_Ascension'Img & " E:" &
               Obliquity_Ecliptic (Date)'Img);
      Ecliptic_Latitude := To_Ecliptic_Latitude (Declination, Right_Ascension,
                                                 Date);
      Ecliptic_Longitude := To_Ecliptic_Longitude (Declination, Right_Ascension,
                                                   Date);
      Put_Line ("Converted back Ecliptic Latitude:" & Ecliptic_Latitude'Img &
                  " Ecliptic Longitude:" & Ecliptic_Longitude'Img);
   end To_Equatorial_ELaELoD;

   procedure To_Ecliptic is

      Declination : Declinations;
      Right_Ascension : Right_Ascensions;
      R_A : Times;
      Date : Dates;
      Ecliptic_Latitude : Ecliptic_Latitudes;
      Ecliptic_Longitude : Ecliptic_Longitudes;

   begin -- To_Ecliptic
      Put ("Declination: ");
      Declination_IO.Get (Declination);
      Put ("Right Ascension in HH MM SS format: ");
      Hour_IO.Get (R_A.Hour);
      Minute_IO.Get (R_A.Minute);
      Second_IO.Get (R_A.Second);
      Right_Ascension := Right_Ascensions (To_Hours (R_A));
      Put ("Date in DD MM YYYY format: ");
      Day_IO.Get (Date.Day);
      Month_IO.Get (Date.Month);
      Year_IO.Get (Date.Year);
      Ecliptic_Latitude := To_Ecliptic_Latitude (Declination, Right_Ascension,
                                                 Date);
      Ecliptic_Longitude := To_Ecliptic_Longitude (Declination, Right_Ascension,
                                                   Date);
      Put_Line ("Ecliptic Latitude:" & Ecliptic_Latitude'Img &
                  "Ecliptic Longitude:" & Ecliptic_Longitude'Img & " E:" &
                  Obliquity_Ecliptic (Date)'Img);
      Declination := To_Declination (Ecliptic_Latitude, Ecliptic_Longitude,
                                     Date);
      Right_Ascension := To_Right_Ascension (Ecliptic_Latitude,
                                             Ecliptic_Longitude, Date);
      Put_Line ("Converted back Declination:" & Declination'Img &
                  " Right Ascension:" & Right_Ascension'Img);
   end To_Ecliptic;

   procedure Precession is

      Declination_Old, Declination_New : Declinations;
      Right_Ascension_Old, Right_Ascension_New : Right_Ascensions;
      R_A : Times;
      Epoch_Old, Epoch_New : Dates;


   begin -- Precession
      Put ("Declination: ");
      Declination_IO.Get (Declination_Old);
      Put ("Right Ascension in HH MM SS format: ");
      Hour_IO.Get (R_A.Hour);
      Minute_IO.Get (R_A.Minute);
      Second_IO.Get (R_A.Second);
      Right_Ascension_Old :=  Right_Ascensions (To_Hours (R_A));
      Put ("Old Epoch in DD MM YYYY format: ");
      Day_IO.Get (Epoch_Old.Day);
      Month_IO.Get (Epoch_Old.Month);
      Year_IO.Get (Epoch_Old.Year);
      Put ("New Epoch in DD MM YYYY format: ");
      Day_IO.Get (Epoch_New.Day);
      Month_IO.Get (Epoch_New.Month);
      Year_IO.Get (Epoch_New.Year);
      Precession_Correction (Declination_Old, Right_Ascension_Old,
                             Epoch_Old, Epoch_New,
                             Declination_New, Right_Ascension_New);
      Put_Line ("new epoch Declination:" & Declination_New'Img &
                  " Right Ascension:" & Right_Ascension_New'Img);

   end Precession;

   procedure Chapter_4_Exercises is

      function Time_String (Time : in Decimal_Hours) return String is
         T : constant Times := To_HHMMSS (Time);

      begin -- Time_String
         return T.Hour'Img & ":" & T.Minute'Img & ":" & T.Second'Img;
      end Time_String;

      function Angle_String (Degree_In : in Degrees) return String is

         Degree : Degrees := Degree_In;
         D : DDDMMSSs;

      begin -- Angle_String
         if Degree < 0.0 then
            Degree := abs (Degree);
            D := To_DDDMMSS (Degree);
            return " -" & D.Degree'Img & D.Minute'Img & D.Second'Img;
         else
            D := To_DDDMMSS (Degree);
            return D.Degree'Img & D.Minute'Img & D.Second'Img;
         end if; -- Degree < 0.0
      end Angle_String;

      Altitude : Altitudes;
      Azimuth : Azimuths;
      Dec : Declinations;
      A_H, R_A : Right_Ascensions;
      Date : Dates;
      Lat : Latitudes;
      Long : Longitudes;
      LST, GST : Decimal_Hours;
      Ec_Lat : Ecliptic_Latitudes;
      Ec_Long : Ecliptic_Longitudes;

   begin -- Chapter_4_Exercises
      Long := (West, 64.0);
      Date := (1976, 6, 5);
      A_H := Right_Ascensions(To_Hours ((15, 30, 15)));
      GST := UTC_To_GST (Date, (14, 0, 0));
      LST := GST;
      To_Local (To_Time_Offset (Long), Date, LST);
      R_A := To_Right_Ascension (LST, A_H);
      Put_Line ("(1) Right Ascension:" & Time_String (Decimal_Hours (R_A)));
      Long := (East, 40.0);
      Date := (2015, 1, 5);
      R_A := Right_Ascensions (To_Hours ((12, 32, 06)));
      GST := UTC_To_GST (Date, (12, 0, 0));
      LST := GST;
      To_Local (To_Time_Offset (Long), Date, LST);
      A_H := To_Hour_Angle (LST, R_A);
      Put_Line ("(2) Hour Angle:" & Time_String (Decimal_Hours (A_H)));
      Altitude := Altitudes (To_Degrees ((10, 0, 0)));
      Azimuth := Azimuths (To_Degrees ((200, 10, 20)));
      Lat := (North, 35.6);
      Dec := To_Declination (Altitude, Azimuth, Lat);
      A_H := To_Hour_Angle (Altitude, Azimuth, Lat);
      Put_Line ("(3) Hour Angle:" & Time_String (Decimal_Hours (A_H)) &
                  " Declination:" & Angle_String (Degrees (Dec)));
      A_H := Right_Ascensions(To_Hours ((7, 0, 0)));
      Dec := Declinations (To_Degrees ((49, 54, 20)));
      Lat := (South, 80.0);
      Altitude := To_Altitude (Dec, A_H, Lat);
      Azimuth := To_Azimuth (Dec, A_H, Lat);
      Put_Line ("(4) Altitude:" & Angle_String (Degrees (Altitude)) &
                  " Azimuth:" & Angle_String (Degrees (Azimuth)));
      Ec_Lat := 0.0;
      Ec_Long := Ecliptic_Longitudes (To_Degrees ((120, 30, 30)));
      Date := (2000, 1, 1);
      Dec := To_Declination (Ec_Lat, Ec_Long, Date);
      R_A := To_Right_Ascension (Ec_Lat, Ec_Long, Date);
      Put_Line ("(5) Right Ascension:" & Time_String (Decimal_Hours (R_A)) &
                  " Declination:" & Angle_String (Degrees (Dec)));
      R_A := Right_Ascensions (To_Hours ((11, 10, 13)));
      Dec := Declinations (To_Degrees ((30, 5, 40)));
      Date := (2000, 1, 1);
      Ec_Lat := To_Ecliptic_Latitude (Dec, R_A, Date);
      Ec_Long := To_Ecliptic_Longitude (Dec, R_A, Date);
      Put_Line ("(6) Ecliptic Latitude:" & Angle_String (Degrees (Ec_Lat)) &
                  " Ecliptic Longitude:" & Angle_String (Degrees (Ec_Long)));
      Put_Line
        ("Exercise 7 to 11 not solved, Galactic Coordinates not implemented");
      R_A := Right_Ascensions (To_Hours ((12, 34, 34)));
      Dec := Declinations (To_Degrees ((20, 49, 8)));
      Precession_Correction (Dec, R_A, (2000, 1, 1), (2015, 1, 1), Dec, R_A);
      Put_Line ("(12) Right Ascension:" & Time_String (Decimal_Hours (R_A)) &
                  " Declination:" & Angle_String (Degrees (Dec)));
      Put_Line ("For exercise 13 to 15 use Test_Orbit test 'Z'");
   end Chapter_4_Exercises;

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
      Put_Line
        ("K To Equatorial (Ecliptic Latitude, Ecliptic Longitude, Date)");
      Put_Line ("L To Ecliptic (Declination, Right Ascension, Date)");
      Put_line ("M Precession Correction");
      Put_Line ("Z Answers to chapter 4 exercises");
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
         when 'K' =>
            To_Equatorial_ELaELoD;
         when 'L' =>
            To_Ecliptic;
         when 'M' =>
            Precession;
         when 'Z' =>
            Chapter_4_Exercises;
         when others =>
            Put_Line ("Unknown test: '" & Test & "'");
      end case; -- Test
   end loop; -- Perform one test
end Test_Coordinates;
