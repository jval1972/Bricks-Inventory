//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2019 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//    Parts description compress
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_description_compress;

interface

type
  DC_compressor_item_t = record
    charcode: byte;
    aliasword: string[14];
  end;
  DC_compressor_item_p = ^DC_compressor_item_t;

  DC_Dictionary_t = array[0..255] of DC_compressor_item_t;
  DC_Dictionary_p = ^DC_Dictionary_t;

const
  DC_dictionary_desc1: DC_Dictionary_t = (
    (charcode:   0; aliasword: #0),
    (charcode:   1; aliasword: 'with'),
    (charcode:   2; aliasword: 'Pattern'),
    (charcode:   3; aliasword: 'Sticker'),
    (charcode:   4; aliasword: 'and'),
    (charcode:   5; aliasword: 'Print'),
    (charcode:   6; aliasword: 'Instructions'),
    (charcode:   7; aliasword: 'Original'),
    (charcode:   8; aliasword: 'Black'),
    (charcode:   9; aliasword: 'White'),
    (charcode:  10; aliasword: #10),
    (charcode:  11; aliasword: 'Torso'),
    (charcode:  12; aliasword: 'Dark'),
    (charcode:  13; aliasword: #13),
    (charcode:  14; aliasword: 'Set'),
    (charcode:  15; aliasword: 'Minifig'),
    (charcode:  16; aliasword: 'Yellow'),
    (charcode:  17; aliasword: 'Blue'),
    (charcode:  18; aliasword: 'Tile'),
    (charcode:  19; aliasword: 'Hands'),
    (charcode:  20; aliasword: 'Light'),
    (charcode:  21; aliasword: 'Brick'),
    (charcode:  22; aliasword: 'for'),
    (charcode:  23; aliasword: 'Minifigure'),
    (charcode:  24; aliasword: 'Background'),
    (charcode:  25; aliasword: 'Silver'),
    (charcode:  26; aliasword: 'Red'),
    (charcode:  27; aliasword: 'Orange'),
    (charcode:  28; aliasword: 'Duplo'),
    (charcode:  29; aliasword: 'Bluish'),
    (charcode:  30; aliasword: 'Stripes'),
    (charcode:  31; aliasword: 'Medium'),
    (charcode:  32; aliasword: #32),
    (charcode:  33; aliasword: #33),
    (charcode:  34; aliasword: 'box'),
    (charcode:  35; aliasword: #35),
    (charcode:  36; aliasword: #36),
    (charcode:  37; aliasword: #37),
    (charcode:  38; aliasword: #38),
    (charcode:  39; aliasword: #39),
    (charcode:  40; aliasword: #40),
    (charcode:  41; aliasword: #41),
    (charcode:  42; aliasword: #42),
    (charcode:  43; aliasword: #43),
    (charcode:  44; aliasword: #44),
    (charcode:  45; aliasword: #45),
    (charcode:  46; aliasword: #46),
    (charcode:  47; aliasword: #47),
    (charcode:  48; aliasword: #48),
    (charcode:  49; aliasword: #49),
    (charcode:  50; aliasword: #50),
    (charcode:  51; aliasword: #51),
    (charcode:  52; aliasword: #52),
    (charcode:  53; aliasword: #53),
    (charcode:  54; aliasword: #54),
    (charcode:  55; aliasword: #55),
    (charcode:  56; aliasword: #56),
    (charcode:  57; aliasword: #57),
    (charcode:  58; aliasword: #58),
    (charcode:  59; aliasword: #59),
    (charcode:  60; aliasword: #60),
    (charcode:  61; aliasword: #61),
    (charcode:  62; aliasword: #62),
    (charcode:  63; aliasword: #63),
    (charcode:  64; aliasword: #64),
    (charcode:  65; aliasword: #65),
    (charcode:  66; aliasword: #66),
    (charcode:  67; aliasword: #67),
    (charcode:  68; aliasword: #68),
    (charcode:  69; aliasword: #69),
    (charcode:  70; aliasword: #70),
    (charcode:  71; aliasword: #71),
    (charcode:  72; aliasword: #72),
    (charcode:  73; aliasword: #73),
    (charcode:  74; aliasword: #74),
    (charcode:  75; aliasword: #75),
    (charcode:  76; aliasword: #76),
    (charcode:  77; aliasword: #77),
    (charcode:  78; aliasword: #78),
    (charcode:  79; aliasword: #79),
    (charcode:  80; aliasword: #80),
    (charcode:  81; aliasword: #81),
    (charcode:  82; aliasword: #82),
    (charcode:  83; aliasword: #83),
    (charcode:  84; aliasword: #84),
    (charcode:  85; aliasword: #85),
    (charcode:  86; aliasword: #86),
    (charcode:  87; aliasword: #87),
    (charcode:  88; aliasword: #88),
    (charcode:  89; aliasword: #89),
    (charcode:  90; aliasword: #90),
    (charcode:  91; aliasword: #91),
    (charcode:  92; aliasword: #92),
    (charcode:  93; aliasword: #93),
    (charcode:  94; aliasword: #94),
    (charcode:  95; aliasword: #95),
    (charcode:  96; aliasword: #96),
    (charcode:  97; aliasword: #97),
    (charcode:  98; aliasword: #98),
    (charcode:  99; aliasword: #99),
    (charcode: 100; aliasword: #100),
    (charcode: 101; aliasword: #101),
    (charcode: 102; aliasword: #102),
    (charcode: 103; aliasword: #103),
    (charcode: 104; aliasword: #104),
    (charcode: 105; aliasword: #105),
    (charcode: 106; aliasword: #106),
    (charcode: 107; aliasword: #107),
    (charcode: 108; aliasword: #108),
    (charcode: 109; aliasword: #109),
    (charcode: 110; aliasword: #110),
    (charcode: 111; aliasword: #111),
    (charcode: 112; aliasword: #112),
    (charcode: 113; aliasword: #113),
    (charcode: 114; aliasword: #114),
    (charcode: 115; aliasword: #115),
    (charcode: 116; aliasword: #116),
    (charcode: 117; aliasword: #117),
    (charcode: 118; aliasword: #118),
    (charcode: 119; aliasword: #119),
    (charcode: 120; aliasword: #120),
    (charcode: 121; aliasword: #121),
    (charcode: 122; aliasword: #122),
    (charcode: 123; aliasword: #123),
    (charcode: 124; aliasword: #124),
    (charcode: 125; aliasword: #125),
    (charcode: 126; aliasword: #126),
    (charcode: 127; aliasword: 'Gray'),
    (charcode: 128; aliasword: 'Head'),
    (charcode: 129; aliasword: 'Brown'),
    (charcode: 130; aliasword: 'Green'),
    (charcode: 131; aliasword: 'Curved'),
    (charcode: 132; aliasword: 'Eyebrows'),
    (charcode: 133; aliasword: 'Technic'),
    (charcode: 134; aliasword: 'Complete'),
    (charcode: 135; aliasword: 'Slope'),
    (charcode: 136; aliasword: 'Ninjago'),
    (charcode: 137; aliasword: 'Shirt'),
    (charcode: 138; aliasword: 'Modified'),
    (charcode: 139; aliasword: 'Right'),
    (charcode: 140; aliasword: 'Friends'),
    (charcode: 141; aliasword: 'Card'),
    (charcode: 142; aliasword: 'Trading'),
    (charcode: 143; aliasword: 'Gold'),
    (charcode: 144; aliasword: 'Jacket'),
    (charcode: 145; aliasword: 'Large'),
    (charcode: 146; aliasword: 'Side'),
    (charcode: 147; aliasword: 'Stickers'),
    (charcode: 148; aliasword: 'Bright'),
    (charcode: 149; aliasword: 'Model'),
    (charcode: 150; aliasword: 'Female'),
    (charcode: 151; aliasword: 'Series'),
    (charcode: 152; aliasword: 'Studs'),
    (charcode: 153; aliasword: 'Stud'),
    (charcode: 154; aliasword: 'Reddish'),
    (charcode: 155; aliasword: 'Nougat'),
    (charcode: 156; aliasword: 'Hips'),
    (charcode: 157; aliasword: 'Panel'),
    (charcode: 158; aliasword: 'Logo'),
    (charcode: 159; aliasword: 'Armor'),
    (charcode: 160; aliasword: 'Left'),
    (charcode: 161; aliasword: 'polybag'),
    (charcode: 162; aliasword: 'Belt'),
    (charcode: 163; aliasword: 'Round'),
    (charcode: 164; aliasword: 'Magazine'),
    (charcode: 165; aliasword: 'German'),
    (charcode: 166; aliasword: #166),
    (charcode: 167; aliasword: 'Groove'),
    (charcode: 168; aliasword: 'print'),
    (charcode: 169; aliasword: 'Bottom'),
    (charcode: 170; aliasword: 'Bionicle'),
    (charcode: 171; aliasword: 'Helmet'),
    (charcode: 172; aliasword: #172),
    (charcode: 173; aliasword: 'Star'),
    (charcode: 174; aliasword: 'Game'),
    (charcode: 175; aliasword: 'on'),
    (charcode: 176; aliasword: 'Recessed'),
    (charcode: 177; aliasword: 'Eyes'),
    (charcode: 178; aliasword: 'Lines'),
    (charcode: 179; aliasword: 'Purple'),
    (charcode: 180; aliasword: 'Calendar'),
    (charcode: 181; aliasword: 'Figure'),
    (charcode: 182; aliasword: 'Hair'),
    (charcode: 183; aliasword: 'Lego'),
    (charcode: 184; aliasword: #184),
    (charcode: 185; aliasword: 'LEGO'),
    (charcode: 186; aliasword: #186),
    (charcode: 187; aliasword: 'Accessories'),
    (charcode: 188; aliasword: 'without'),
    (charcode: 189; aliasword: 'Buttons'),
    (charcode: 190; aliasword: #190),
    (charcode: 191; aliasword: 'Hollow'),
    (charcode: 192; aliasword: 'Sheet'),
    (charcode: 193; aliasword: #193),
    (charcode: 194; aliasword: #194),
    (charcode: 195; aliasword: #195),
    (charcode: 196; aliasword: #196),
    (charcode: 197; aliasword: #197),
    (charcode: 198; aliasword: #198),
    (charcode: 199; aliasword: #199),
    (charcode: 200; aliasword: #200),
    (charcode: 201; aliasword: #201),
    (charcode: 202; aliasword: #202),
    (charcode: 203; aliasword: #203),
    (charcode: 204; aliasword: #204),
    (charcode: 205; aliasword: #205),
    (charcode: 206; aliasword: #206),
    (charcode: 207; aliasword: #207),
    (charcode: 208; aliasword: #208),
    (charcode: 209; aliasword: #209),
    (charcode: 210; aliasword: 'Open'),
    (charcode: 211; aliasword: #211),
    (charcode: 212; aliasword: #212),
    (charcode: 213; aliasword: #213),
    (charcode: 214; aliasword: #214),
    (charcode: 215; aliasword: #215),
    (charcode: 216; aliasword: #216),
    (charcode: 217; aliasword: #217),
    (charcode: 218; aliasword: 'Pockets'),
    (charcode: 219; aliasword: 'Top'),
    (charcode: 220; aliasword: #220),
    (charcode: 221; aliasword: #221),
    (charcode: 222; aliasword: #222),
    (charcode: 223; aliasword: #223),
    (charcode: 224; aliasword: 'Supports'),
    (charcode: 225; aliasword: #225),
    (charcode: 226; aliasword: #226),
    (charcode: 227; aliasword: #227),
    (charcode: 228; aliasword: #228),
    (charcode: 229; aliasword: #229),
    (charcode: 230; aliasword: #230),
    (charcode: 231; aliasword: #231),
    (charcode: 232; aliasword: #232),
    (charcode: 233; aliasword: 'Modulex'),
    (charcode: 234; aliasword: #234),
    (charcode: 235; aliasword: #235),
    (charcode: 236; aliasword: #236),
    (charcode: 237; aliasword: #237),
    (charcode: 238; aliasword: #238),
    (charcode: 239; aliasword: #239),
    (charcode: 240; aliasword: #240),
    (charcode: 241; aliasword: #241),
    (charcode: 242; aliasword: #242),
    (charcode: 243; aliasword: #243),
    (charcode: 244; aliasword: #244),
    (charcode: 245; aliasword: #245),
    (charcode: 246; aliasword: #246),
    (charcode: 247; aliasword: #247),
    (charcode: 248; aliasword: #248),
    (charcode: 249; aliasword: 'Arms'),
    (charcode: 250; aliasword: #250),
    (charcode: 251; aliasword: #251),
    (charcode: 252; aliasword: #252),
    (charcode: 253; aliasword: #253),
    (charcode: 254; aliasword: #254),
    (charcode: 255; aliasword: #255)
  );

  DC_dictionary_desc2: DC_Dictionary_t = (
    (charcode:   0; aliasword: #0),
    (charcode:   1; aliasword: 'Mouth'),
    (charcode:   2; aliasword: 'Small'),
    (charcode:   3; aliasword: 'Smile'),
    (charcode:   4; aliasword: 'Pink'),
    (charcode:   5; aliasword: 'Plate'),
    (charcode:   6; aliasword: 'Legoland'),
    (charcode:   7; aliasword: 'Mini'),
    (charcode:   8; aliasword: 'Turquoise'),
    (charcode:   9; aliasword: 'Lavender'),
    (charcode:  10; aliasword: #10),
    (charcode:  11; aliasword: 'Magenta'),
    (charcode:  12; aliasword: 'Police'),
    (charcode:  13; aliasword: #13),
    (charcode:  14; aliasword: 'Sided'),
    (charcode:  15; aliasword: 'Batman'),
    (charcode:  16; aliasword: 'Back'),
    (charcode:  17; aliasword: 'Edition'),
    (charcode:  18; aliasword: 'Short'),
    (charcode:  19; aliasword: 'Stripe'),
    (charcode:  20; aliasword: 'Version'),
    (charcode:  21; aliasword: 'Exclusive'),
    (charcode:  22; aliasword: 'Collar'),
    (charcode:  23; aliasword: 'Activity'),
    (charcode:  24; aliasword: 'Wedge'),
    (charcode:  25; aliasword: 'Train'),
    (charcode:  26; aliasword: 'Display'),
    (charcode:  27; aliasword: 'Issue'),
    (charcode:  28; aliasword: 'Advent'),
    (charcode:  29; aliasword: 'Sides'),
    (charcode:  30; aliasword: 'Number'),
    (charcode:  31; aliasword: 'Front'),
    (charcode:  32; aliasword: 'Pack'),
    (charcode:  33; aliasword: 'World'),
    (charcode:  34; aliasword: 'Windscreen'),
    (charcode:  35; aliasword: 'Dragon'),
    (charcode:  36; aliasword: 'City'),
    (charcode:  37; aliasword: 'Necklace'),
    (charcode:  38; aliasword: 'Vest'),
    (charcode:  39; aliasword: 'Dual'),
    (charcode:  40; aliasword: 'Lime'),
    (charcode:  41; aliasword: 'Holder'),
    (charcode:  42; aliasword: 'Outline'),
    (charcode:  43; aliasword: 'Construction'),
    (charcode:  44; aliasword: 'Square'),
    (charcode:  45; aliasword: 'Teeth'),
    (charcode:  46; aliasword: 'Badge'),
    (charcode:  47; aliasword: 'Fairing'),
    (charcode:  48; aliasword: 'Minidoll'),
    (charcode:  49; aliasword: 'over'),
    (charcode:  50; aliasword: 'Triangle'),
    (charcode:  51; aliasword: 'Plates'),
    (charcode:  52; aliasword: 'Headgear'),
    (charcode:  53; aliasword: 'Stickered'),
    (charcode:  54; aliasword: 'Bricks'),
    (charcode:  55; aliasword: 'Thick'),
    (charcode:  56; aliasword: 'Male'),
    (charcode:  57; aliasword: 'Clikits'),
    (charcode:  58; aliasword: 'Window'),
    (charcode:  59; aliasword: 'Chain'),
    (charcode:  60; aliasword: 'Double'),
    (charcode:  61; aliasword: 'Face'),
    (charcode:  62; aliasword: 'Moustache'),
    (charcode:  63; aliasword: 'Glasses'),
    (charcode:  64; aliasword: 'International'),
    (charcode:  65; aliasword: 'Fire'),
    (charcode:  66; aliasword: 'Shoulder'),
    (charcode:  67; aliasword: 'Potter'),
    (charcode:  68; aliasword: 'Shield'),
    (charcode:  69; aliasword: 'Knights'),
    (charcode:  70; aliasword: 'Circle'),
    (charcode:  71; aliasword: 'Electric'),
    (charcode:  72; aliasword: 'Cloth'),
    (charcode:  73; aliasword: 'Buckle'),
    (charcode:  74; aliasword: 'Spinjitzu'),
    (charcode:  75; aliasword: 'Space'),
    (charcode:  76; aliasword: 'French'),
    (charcode:  77; aliasword: 'Zipper'),
    (charcode:  78; aliasword: 'Smooth'),
    (charcode:  79; aliasword: 'Promotional'),
    (charcode:  80; aliasword: 'Inverted'),
    (charcode:  81; aliasword: 'Trooper'),
    (charcode:  82; aliasword: 'Liftarm'),
    (charcode:  83; aliasword: 'Sweater'),
    (charcode:  84; aliasword: 'Doll'),
    (charcode:  85; aliasword: 'Super'),
    (charcode:  86; aliasword: 'Danger'),
    (charcode:  87; aliasword: 'Cardboard'),
    (charcode:  88; aliasword: 'Container'),
    (charcode:  89; aliasword: 'Center'),
    (charcode:  90; aliasword: 'Metallic'),
    (charcode:  91; aliasword: 'Plastic'),
    (charcode:  92; aliasword: 'Spider'),
    (charcode:  93; aliasword: 'Classic'),
    (charcode:  94; aliasword: 'Lightning'),
    (charcode:  95; aliasword: 'Skirt'),
    (charcode:  96; aliasword: 'Undershirt'),
    (charcode:  97; aliasword: 'NINJAGO'),
    (charcode:  98; aliasword: 'Special'),
    (charcode:  99; aliasword: 'Sleeves'),
    (charcode: 100; aliasword: 'Stand'),
    (charcode: 101; aliasword: 'Internal'),
    (charcode: 102; aliasword: 'Alien'),
    (charcode: 103; aliasword: 'Holes'),
    (charcode: 104; aliasword: 'Straps'),
    (charcode: 105; aliasword: 'Markings'),
    (charcode: 106; aliasword: 'Sunglasses'),
    (charcode: 107; aliasword: 'Building'),
    (charcode: 108; aliasword: 'Harry'),
    (charcode: 109; aliasword: 'Utensil'),
    (charcode: 110; aliasword: 'Trans'),
    (charcode: 111; aliasword: 'Castle'),
    (charcode: 112; aliasword: 'Soccer'),
    (charcode: 113; aliasword: 'Chest'),
    (charcode: 114; aliasword: 'Imperial'),
    (charcode: 115; aliasword: 'Mosaic'),
    (charcode: 116; aliasword: 'Vehicle'),
    (charcode: 117; aliasword: 'Beard'),
    (charcode: 118; aliasword: 'Wheels'),
    (charcode: 119; aliasword: 'Fabuland'),
    (charcode: 120; aliasword: 'Pentagonal'),
    (charcode: 121; aliasword: 'Belville'),
    (charcode: 122; aliasword: 'Pearl'),
    (charcode: 123; aliasword: 'Polish'),
    (charcode: 124; aliasword: 'Minecraft'),
    (charcode: 125; aliasword: 'English'),
    (charcode: 126; aliasword: 'Cylinder'),
    (charcode: 127; aliasword: 'January'),
    (charcode: 128; aliasword: 'Create'),
    (charcode: 129; aliasword: 'Pixelated'),
    (charcode: 130; aliasword: 'Stormtrooper'),
    (charcode: 131; aliasword: 'Challenge'),
    (charcode: 132; aliasword: 'Circles'),
    (charcode: 133; aliasword: 'Triple'),
    (charcode: 134; aliasword: 'Overalls'),
    (charcode: 135; aliasword: 'Pupils'),
    (charcode: 136; aliasword: 'Power'),
    (charcode: 137; aliasword: 'Flowers'),
    (charcode: 138; aliasword: 'Christmas'),
    (charcode: 139; aliasword: 'Circuitry'),
    (charcode: 140; aliasword: 'Stars'),
    (charcode: 141; aliasword: 'Handle'),
    (charcode: 142; aliasword: 'Inside'),
    (charcode: 143; aliasword: 'December'),
    (charcode: 144; aliasword: 'Pocket'),
    (charcode: 145; aliasword: 'Truck'),
    (charcode: 146; aliasword: 'Jurassic'),
    (charcode: 147; aliasword: 'Collection'),
    (charcode: 148; aliasword: 'Wheel'),
    (charcode: 149; aliasword: 'Control'),
    (charcode: 150; aliasword: 'Scala'),
    (charcode: 151; aliasword: 'Flower'),
    (charcode: 152; aliasword: 'Picture'),
    (charcode: 153; aliasword: 'Helicopter'),
    (charcode: 154; aliasword: 'Storage'),
    (charcode: 155; aliasword: 'Heart'),
    (charcode: 156; aliasword: 'Checkered'),
    (charcode: 157; aliasword: 'Boots'),
    (charcode: 158; aliasword: 'Build'),
    (charcode: 159; aliasword: 'Minifigures'),
    (charcode: 160; aliasword: 'Headlights'),
    (charcode: 161; aliasword: 'Eyelashes'),
    (charcode: 162; aliasword: 'Rounded'),
    (charcode: 163; aliasword: 'Store'),
    (charcode: 164; aliasword: 'Motorcycle'),
    (charcode: 165; aliasword: 'Tubes'),
    (charcode: 166; aliasword: 'Stubble'),
    (charcode: 167; aliasword: 'Vertical'),
    (charcode: 168; aliasword: 'Closed'),
    (charcode: 169; aliasword: 'Scales'),
    (charcode: 170; aliasword: 'Dutch'),
    (charcode: 171; aliasword: 'Raised'),
    (charcode: 172; aliasword: 'Poster'),
    (charcode: 173; aliasword: 'Baseplate'),
    (charcode: 174; aliasword: 'Striped'),
    (charcode: 175; aliasword: 'Pilot'),
    (charcode: 176; aliasword: 'Breastplate'),
    (charcode: 177; aliasword: 'Reflective'),
    (charcode: 178; aliasword: 'Battle'),
    (charcode: 179; aliasword: 'Muscles'),
    (charcode: 180; aliasword: 'Masters'),
    (charcode: 181; aliasword: 'Grille'),
    (charcode: 182; aliasword: 'Connector'),
    (charcode: 183; aliasword: 'Screen'),
    (charcode: 184; aliasword: 'Pirate'),
    (charcode: 185; aliasword: 'Dress'),
    (charcode: 186; aliasword: 'Dimensions'),
    (charcode: 187; aliasword: 'Skull'),
    (charcode: 188; aliasword: 'American'),
    (charcode: 189; aliasword: 'Characters'),
    (charcode: 190; aliasword: 'Cross'),
    (charcode: 191; aliasword: 'Backpack'),
    (charcode: 192; aliasword: 'Triangles'),
    (charcode: 193; aliasword: 'Ponytail'),
    (charcode: 194; aliasword: 'Factory'),
    (charcode: 195; aliasword: 'Angry'),
    (charcode: 196; aliasword: 'Movie'),
    (charcode: 197; aliasword: 'Captain'),
    (charcode: 198; aliasword: 'Spots'),
    (charcode: 199; aliasword: 'Letter'),
    (charcode: 200; aliasword: 'Flames'),
    (charcode: 201; aliasword: 'Cover'),
    (charcode: 202; aliasword: 'LEGOLAND'),
    (charcode: 203; aliasword: 'Logogram'),
    (charcode: 204; aliasword: 'Postcard'),
    (charcode: 205; aliasword: 'Character'),
    (charcode: 206; aliasword: 'Frame'),
    (charcode: 207; aliasword: 'Ferrari'),
    (charcode: 208; aliasword: 'Animal'),
    (charcode: 209; aliasword: 'Mechanical'),
    (charcode: 210; aliasword: 'Border'),
    (charcode: 211; aliasword: 'Dinosaur'),
    (charcode: 212; aliasword: 'Panels'),
    (charcode: 213; aliasword: 'Legends'),
    (charcode: 214; aliasword: 'Guard'),
    (charcode: 215; aliasword: 'Clone'),
    (charcode: 216; aliasword: 'POLICE'),
    (charcode: 217; aliasword: 'Visor'),
    (charcode: 218; aliasword: 'Sleeve'),
    (charcode: 219; aliasword: 'Horse'),
    (charcode: 220; aliasword: 'Starfighter'),
    (charcode: 221; aliasword: 'Corner'),
    (charcode: 222; aliasword: 'Shuttle'),
    (charcode: 223; aliasword: 'Deutschland'),
    (charcode: 224; aliasword: 'eacute'),
    (charcode: 225; aliasword: 'Rectangle'),
    (charcode: 226; aliasword: 'Cheek'),
    (charcode: 227; aliasword: 'Utility'),
    (charcode: 228; aliasword: 'Horizontal'),
    (charcode: 229; aliasword: 'Chima'),
    (charcode: 230; aliasword: 'Button'),
    (charcode: 231; aliasword: 'Glass'),
    (charcode: 232; aliasword: 'Yellowish'),
    (charcode: 233; aliasword: 'Hinge'),
    (charcode: 234; aliasword: 'Plain'),
    (charcode: 235; aliasword: 'Adventure'),
    (charcode: 236; aliasword: 'Triangular'),
    (charcode: 237; aliasword: 'Flesh'),
    (charcode: 238; aliasword: 'Player'),
    (charcode: 239; aliasword: 'Skywalker'),
    (charcode: 240; aliasword: 'Freckles'),
    (charcode: 241; aliasword: 'Droid'),
    (charcode: 242; aliasword: 'Uniform'),
    (charcode: 243; aliasword: 'Wars'),
    (charcode: 244; aliasword: 'Trousers'),
    (charcode: 245; aliasword: 'Rivets'),
    (charcode: 246; aliasword: 'Officer'),
    (charcode: 247; aliasword: 'Hogwarts'),
    (charcode: 248; aliasword: 'Squares'),
    (charcode: 249; aliasword: 'Jumpsuit'),
    (charcode: 250; aliasword: 'Straight'),
    (charcode: 251; aliasword: 'Wrinkles'),
    (charcode: 252; aliasword: 'Mudguard'),
    (charcode: 253; aliasword: 'Azure'),
    (charcode: 254; aliasword: 'Legs'),
    (charcode: 255; aliasword: #255)
  );

const
  DC_NUM_REPLACE_STR = 224;

type
  DC_replace_item_t = record
    wordcode: string[3];
    aliasword: string[45];
    len: integer;
  end;
  DC_replace_item_p = ^DC_replace_item_t;

  DC_Replace_t = array[0..DC_NUM_REPLACE_STR - 1] of DC_replace_item_t;
  DC_Replace_p = ^DC_Replace_t;

const
  DC_Replace: DC_Replace_t = (
    (wordcode:   #255#255#0; aliasword: #0),
    (wordcode:   #255#255#1; aliasword: ' Service Packs '),
    (wordcode:   #255#255#2; aliasword: ' (architectural hobby und modelbau version)'),
    (wordcode:   #255#255#3; aliasword: ' Body Part '),
    (wordcode:   #255#255#4; aliasword: 'Antenna Whip '),
    (wordcode:   #255#255#5; aliasword: 'Arctic Explorer '),
    (wordcode:   #255#255#6; aliasword: 'Bicycle Heavy Mountain Bike '),
    (wordcode:   #255#255#7; aliasword: 'Indiana Jones '),
    (wordcode:   #255#255#8; aliasword: 'McDonald''s Racers Car '),
    (wordcode:   #255#255#9; aliasword: 'McDonald''s Sports '),
    (wordcode:  #255#255#10; aliasword: #10),
    (wordcode:  #255#255#11; aliasword: 'Memo Pad '),
    (wordcode:  #255#255#12; aliasword: 'Mercedes AMG Petronas Formula One '),
    (wordcode:  #255#255#13; aliasword: #13),
    (wordcode:  #255#255#14; aliasword: 'Mickey Mouse '),
    (wordcode:  #255#255#15; aliasword: 'Microfig Heroica '),
    (wordcode:  #255#255#16; aliasword: 'Microfig Magma Monster'),
    (wordcode:  #255#255#17; aliasword: 'Microfig Minotaurus Gladiator '),
    (wordcode:  #255#255#18; aliasword: 'Microfig Orient Bazaar Merchant '),
    (wordcode:  #255#255#19; aliasword: 'Microfig Ramses Pyramid Adventurer '),
    (wordcode:  #255#255#20; aliasword: 'Mighty Micros: '),
    (wordcode:  #255#255#21; aliasword: 'Millennium Falcon'),
    (wordcode:  #255#255#22; aliasword: 'Mindstorms Education '),
    (wordcode:  #255#255#23; aliasword: 'NXT Software '),
    (wordcode:  #255#255#24; aliasword: 'Mindstorms RCX '),
    (wordcode:  #255#255#25; aliasword: 'MINI HEAD '),
    (wordcode:  #255#255#26; aliasword: 'MINI LOWER PART '),
    (wordcode:  #255#255#27; aliasword: 'MINI UPPER PART '),
    (wordcode:  #255#255#28; aliasword: ' Asymmetrical Layered '),
    (wordcode:  #255#255#29; aliasword: ' Full Length '),
    (wordcode:  #255#255#30; aliasword: ' Leg Protection'),
    (wordcode:  #255#255#31; aliasword: 'Wooden Pull Along '),
    (wordcode:  #255#255#32; aliasword: ' Club Magazin ('),
    (wordcode:  #255#255#33; aliasword: ' 4 x 4 x 1 '),
    (wordcode:  #255#255#34; aliasword: ' 2 x 4 x 1 '),
    (wordcode:  #255#255#35; aliasword: ' 10 x 6 x 3 Bubble Canopy '),
    (wordcode:  #255#255#36; aliasword: 'Wildlife Rescue '),
    (wordcode:  #255#255#37; aliasword: 'Wild Animals of '),
    (wordcode:  #255#255#38; aliasword: ' 400 Ideas Leaflet '),
    (wordcode:  #255#255#39; aliasword: ' Mario Scanner Code '),
    (wordcode:  #255#255#40; aliasword: 'Chocolate Frog '),
    (wordcode:  #255#255#41; aliasword: ' BeatBit Album '),
    (wordcode:  #255#255#42; aliasword: ' Evolution of the '),
    (wordcode:  #255#255#43; aliasword: ' Evolution Of The '),
    (wordcode:  #255#255#44; aliasword: ' Computer Monitor '),
    (wordcode:  #255#255#45; aliasword: ' 1 x 1 '),
    (wordcode:  #255#255#46; aliasword: ' 1 x 2 '),
    (wordcode:  #255#255#47; aliasword: ' 1 x 3 '),
    (wordcode:  #255#255#48; aliasword: ' 1 x 4 '),
    (wordcode:  #255#255#49; aliasword: ' 1 x 6 '),
    (wordcode:  #255#255#50; aliasword: ' 1 x 8 '),
    (wordcode:  #255#255#51; aliasword: ' 2 x 2 '),
    (wordcode:  #255#255#52; aliasword: ' 2 x 4 '),
    (wordcode:  #255#255#53; aliasword: ' 2 x 3 '),
    (wordcode:  #255#255#54; aliasword: ' 3 x 3 '),
    (wordcode:  #255#255#55; aliasword: ' 4 x 4 '),
    (wordcode:  #255#255#56; aliasword: ' 1 x 9 Bent (6 - 4) '),
    (wordcode:  #255#255#57; aliasword: ' 1 x 9 Bent (7 - 3) '),
    (wordcode:  #255#255#58; aliasword: ' Flex Cable '),
    (wordcode:  #255#255#59; aliasword: 'Block 3 x 5 x 1 2/3'),
    (wordcode:  #255#255#60; aliasword: ' Ball Joint '),
    (wordcode:  #255#255#61; aliasword: ' 5 x 11 x 1 '),
    (wordcode:  #255#255#62; aliasword: ' 3 x 11 x 1 '),
    (wordcode:  #255#255#63; aliasword: ' 3 x 5 x 3 '),
    (wordcode:  #255#255#64; aliasword: 'Team X-treme Daredevil 3 ('),
    (wordcode:  #255#255#65; aliasword: 'Teacher''s Guide '),
    (wordcode:  #255#255#66; aliasword: 'Tail 4 x 1 x 3 '),
    (wordcode:  #255#255#67; aliasword: 'Tail 12 x 2 x 5 '),
    (wordcode:  #255#255#68; aliasword: 'Sweet Mayhem '),
    (wordcode:  #255#255#69; aliasword: 'Surfboard on Ocean - '),
    (wordcode:  #255#255#70; aliasword: 'Support 2 x 2 x '),
    (wordcode:  #255#255#71; aliasword: 'Support Crane '),
    (wordcode:  #255#255#72; aliasword: ' Heroes Comic Book Marvel '),
    (wordcode:  #255#255#73; aliasword: 'Stuntz Flywheel '),
    (wordcode:  #255#255#74; aliasword: 'Stuntz Driver '),
    (wordcode:  #255#255#75; aliasword: 'String Reel Winch '),
    (wordcode:  #255#255#76; aliasword: 'String Cord'),
    (wordcode:  #255#255#77; aliasword: 'String Reel Winch '),
    (wordcode:  #255#255#78; aliasword: ' Motorized Mechanisms '),
    (wordcode:  #255#255#79; aliasword: 'Art Project 2002 - 0'),
    (wordcode:  #255#255#80; aliasword: ' - Guessing Competition - '),
    (wordcode:  #255#255#81; aliasword: ' - 2004 train postcard 0'),
    (wordcode:  #255#255#82; aliasword: 'Post Office'),
    (wordcode:  #255#255#83; aliasword: 'Porsche 911'),
    (wordcode:  #255#255#84; aliasword: 'Playing Cards St'),
    (wordcode:  #255#255#85; aliasword: 'Tree Flat '),
    (wordcode:  #255#255#86; aliasword: 'Plane Passenger '),
    (wordcode:  #255#255#87; aliasword: 'PLANE BL. DEC. BIG '),
    (wordcode:  #255#255#88; aliasword: 'Pirates of the Caribbean '),
    (wordcode:  #255#255#89; aliasword: ' Anchor Tattoo '),
    (wordcode:  #255#255#90; aliasword: '2023 - Marvel Avengers (Day '),
    (wordcode:  #255#255#91; aliasword: 'Obi-Wan Kenobi '),
    (wordcode:  #255#255#92; aliasword: ' (Next Level) -'),
    (wordcode:  #255#255#93; aliasword: 'Nestle Promo '),
    (wordcode:  #255#255#94; aliasword: 'My First '),
    (wordcode:  #255#255#95; aliasword: 'Musical Instrument Guitar '),
    (wordcode:  #255#255#96; aliasword: 'Music Builder Sound Plug '),
    (wordcode:  #255#255#97; aliasword: 'Mr. Freeze '),
    (wordcode:  #255#255#98; aliasword: ' Sport Bike '),
    (wordcode:  #255#255#99; aliasword: 'Modell des Monats-Karte - 2'),
    (wordcode: #255#255#100; aliasword: ' Metal Key '),
    (wordcode: #255#255#101; aliasword: ' Computer Laptop '),
    (wordcode: #255#255#102; aliasword: ' Trophy Statuette  '),
    (wordcode: #255#255#103; aliasword: 'Year of the '),
    (wordcode: #255#255#104; aliasword: 'Xtreme Stunts '),
    (wordcode: #255#255#105; aliasword: 'Wooden Pull-Along '),
    (wordcode: #255#255#106; aliasword: 'Wonder Woman '),
    (wordcode: #255#255#107; aliasword: 'Wolf People '),
    (wordcode: #255#255#108; aliasword: 'Winter Village '),
    (wordcode: #255#255#109; aliasword: 'Winter Holiday '),
    (wordcode: #255#255#110; aliasword: ' Knight Quarters '),
    (wordcode: #255#255#111; aliasword: 'Knight Scale Mail '),
    (wordcode: #255#255#112; aliasword: ' TIE Interceptor'),
    (wordcode: #255#255#113; aliasword: ' TIE Fighter'),
    (wordcode: #255#255#114; aliasword: 'Passenger Plane - '),
    (wordcode: #255#255#115; aliasword: ' Ocean Exploration '),
    (wordcode: #255#255#116; aliasword: 'Monster Jam '),
    (wordcode: #255#255#117; aliasword: 'Monkie Kid'),
    (wordcode: #255#255#118; aliasword: 'Monkey King'),
    (wordcode: #255#255#119; aliasword: 'Minnie Mouse'),
    (wordcode: #255#255#120; aliasword: 'Ice Cream '),
    (wordcode: #255#255#121; aliasword: 'Hot Dog '),
    (wordcode: #255#255#122; aliasword: 'HO Scale'),
    (wordcode: #255#255#123; aliasword: 'Iron Man '),
    (wordcode: #255#255#124; aliasword: ' Alpha Team '),
    (wordcode: #255#255#125; aliasword: ' LED Headlamp Torch '),
    (wordcode: #255#255#126; aliasword: 'Harley Quinn '),
    (wordcode: #255#255#127; aliasword: 'Han Solo '),
    (wordcode: #255#255#128; aliasword: 'Food - Water Bottle '),
    (wordcode: #255#255#129; aliasword: 'Food - Party '),
    (wordcode: #255#255#130; aliasword: 'Food - Drink Bottle '),
    (wordcode: #255#255#131; aliasword: 'Food - Cutlery '),
    (wordcode: #255#255#132; aliasword: 'Food - Cup / Mug  '),
    (wordcode: #255#255#133; aliasword: 'FLAT TILE 1X2 '),
    (wordcode: #255#255#134; aliasword: 'FLAT TILE 2X2 '),
    (wordcode: #255#255#135; aliasword: ' Time Teacher '),
    (wordcode: #255#255#136; aliasword: 'Flag on Flagpole'),
    (wordcode: #255#255#137; aliasword: 'Flag 7 x 3 '),
    (wordcode: #255#255#138; aliasword: 'Flag 8 x 5 Wave '),
    (wordcode: #255#255#139; aliasword: 'Flag 6 x 4 '),
    (wordcode: #255#255#140; aliasword: 'Flag 5 x 6 Hexagonal '),
    (wordcode: #255#255#141; aliasword: 'Fantasy Era - Crown Knight Scale Mail '),
    (wordcode: #255#255#142; aliasword: 'Fantasy Era - Dwarf '),
    (wordcode: #255#255#143; aliasword: ' Flaring Intake '),
    (wordcode: #255#255#144; aliasword: ' House Block '),
    (wordcode: #255#255#145; aliasword: ' Pit Crew Member '),
    (wordcode: #255#255#146; aliasword: 'Extreme Team - '),
    (wordcode: #255#255#147; aliasword: 'Explore Junior Builder Board '),
    (wordcode: #255#255#148; aliasword: 'Emperor Palpatine'),
    (wordcode: #255#255#149; aliasword: 'Emily Jones '),
    (wordcode: #255#255#150; aliasword: 'Elf Statue'),
    (wordcode: #255#255#151; aliasword: 'Elite Praetorian '),
    (wordcode: #255#255#152; aliasword: ' 4.5V Battery Box 6 x 11 x 3 Type I'),
    (wordcode: #255#255#153; aliasword: ' 4.5V Battery Box 7 x 11 x 3 Type '),
    (wordcode: #255#255#154; aliasword: 'Easter Bunny'),
    (wordcode: #255#255#155; aliasword: ' Metal Axle '),
    (wordcode: #255#255#156; aliasword: 'Van Type '),
    (wordcode: #255#255#157; aliasword: 'Rear Door '),
    (wordcode: #255#255#158; aliasword: ' Semi-Tractor '),
    (wordcode: #255#255#159; aliasword: ' Steam Engine '),
    (wordcode: #255#255#160; aliasword: 'Toolo MyBot Engine Program'),
    (wordcode: #255#255#161; aliasword: 'Digital Clock '),
    (wordcode: #255#255#162; aliasword: 'Digital Camera '),
    (wordcode: #255#255#163; aliasword: 'Dick Grayson'),
    (wordcode: #255#255#164; aliasword: 'Design News Innovations '),
    (wordcode: #255#255#165; aliasword: 'Denken mit'),
    (wordcode: #255#255#166; aliasword: 'DC Universe'),
    (wordcode: #255#255#167; aliasword: 'Heroes - Phonics Boxed'),
    (wordcode: #255#255#168; aliasword: 'DC Comics '),
    (wordcode: #255#255#169; aliasword: 'Darth Maul'),
    (wordcode: #255#255#170; aliasword: 'Custom minifigure #'),
    (wordcode: #255#255#171; aliasword: 'Crusader Axe '),
    (wordcode: #255#255#172; aliasword: 'Crusader Lion '),
    (wordcode: #255#255#173; aliasword: 'Creator Board '),
    (wordcode: #255#255#174; aliasword: ' Incredible InventionsŽ'),
    (wordcode: #255#255#175; aliasword: 'Crane Harbor '),
    (wordcode: #255#255#176; aliasword: 'Crane Arm Outside'),
    (wordcode: #255#255#177; aliasword: 'Cone Half 8 x 4 x 6'),
    (wordcode: #255#255#178; aliasword: 'Computer Mouse Pad '),
    (wordcode: #255#255#179; aliasword: 'Competition Form '),
    (wordcode: #255#255#180; aliasword: 'Commissioner Gordon '),
    (wordcode: #255#255#181; aliasword: 'Coloring Book '),
    (wordcode: #255#255#182; aliasword: 'Coin Bank'),
    (wordcode: #255#255#183; aliasword: 'Coca-Cola '),
    (wordcode: #255#255#184; aliasword: 'Boat Hull Unitary '),
    (wordcode: #255#255#185; aliasword: 'Boat Bow '),
    (wordcode: #255#255#186; aliasword: 'Rubber Raft '),
    (wordcode: #255#255#187; aliasword: 'Zamor Sphere '),
    (wordcode: #255#255#188; aliasword: 'Weapon Mistika Nynrah Ghost Blaster '),
    (wordcode: #255#255#189; aliasword: 'Weapon Hordika Blazer Claw '),
    (wordcode: #255#255#190; aliasword: ' The Bohrok Awake '),
    (wordcode: #255#255#191; aliasword: 'Rahkshi Kraata Stage '),
    (wordcode: #255#255#192; aliasword: ' - Great Mask Lewa Kanohi '),
    (wordcode: #255#255#193; aliasword: 'Mask of Earth (Unity) '),
    (wordcode: #255#255#194; aliasword: ' Pillowcase (140 x 200 cm) - '),
    (wordcode: #255#255#195; aliasword: ' Pillowcase (135 x 200 cm) - '),
    (wordcode: #255#255#196; aliasword: 'Bead Pen Body '),
    (wordcode: #255#255#197; aliasword: ' Shop At Home - '),
    (wordcode: #255#255#198; aliasword: ' Shell Pendant '),
    (wordcode: #255#255#199; aliasword: 'Messenger Bag'),
    (wordcode: #255#255#200; aliasword: ' Tyrannosaurus Rex'),
    (wordcode: #255#255#201; aliasword: ' Tyrannosaurus rex'),
    (wordcode: #255#255#202; aliasword: ' Living Amazingly '),
    (wordcode: #255#255#203; aliasword: 'Flight Suit'),
    (wordcode: #255#255#204; aliasword: 'Instruction CD-ROM '),
    (wordcode: #255#255#205; aliasword: 'Instruction Floppy Disk '),
    (wordcode: #255#255#206; aliasword: 'General Grievous'),
    (wordcode: #255#255#207; aliasword: 'Various Theme Postcards'),
    (wordcode: #255#255#208; aliasword: '8 x 6 x 2'),
    (wordcode: #255#255#209; aliasword: '6 x 4 x 1 1/3 '),
    (wordcode: #255#255#210; aliasword: '16 x 4 '),
    (wordcode: #255#255#211; aliasword: ' 10 x 3 '),
    (wordcode: #255#255#212; aliasword: 'Viking Warrior'),
    (wordcode: #255#255#213; aliasword: 'Video DVD'),
    (wordcode: #255#255#214; aliasword: 'Trailer Base '),
    (wordcode: #255#255#215; aliasword: 'Turntable 6 x 6 '),
    (wordcode: #255#255#216; aliasword: 'Toys ''R'' Us'),
    (wordcode: #255#255#217; aliasword: 'SW Darth Vader'),
    (wordcode: #255#255#218; aliasword: 'Speed Champions'),
    (wordcode: #255#255#219; aliasword: ' Basketball Jersey  '),
    (wordcode: #255#255#220; aliasword: 'Mobile Phone Accessory  Strap '),
    (wordcode: #255#255#220; aliasword: ' Puzzle Piece'),
    (wordcode: #255#255#220; aliasword: 'Hose Flexible Ribbed'),
    (wordcode: #255#255#220; aliasword: ' Pneumatic 4mm D')
  );


function BI_EncodeDesc(const s: string): string;

function BI_DecodeDesc(const s: string): string;

implementation

uses
  Classes, SysUtils;

var
  dict1, dict2: TStringList;

type
  TNumber = class(Tobject)
    value: integer;
    constructor Create(const v: integer);
  end;

constructor TNumber.Create(const v: integer);
begin
  Inherited Create;
  value := v;
end;

function isAlfaNumeric(const c: char): boolean;
var
  b: byte;
begin
  b := Ord(c);
  Result :=
    ((b >= Ord('a')) and (b <= Ord('z'))) or
    ((b >= Ord('A')) and (b <= Ord('Z'))) or
    ((b >= Ord('0')) and (b <= Ord('9')));
end;

function BI_EncodeDesc(const s: string): string;
var
  i: integer;
  stmp: string;
  sret: string;
  len, len2: integer;

  procedure addtoret;
  var
    idx: integer;
  begin
    if stmp = '' then
      exit;
    idx := dict1.IndexOf(stmp);
    if idx >= 0 then
      sret := sret + Chr((dict1.Objects[idx] as TNumber).value)
    else
    begin
      idx := dict2.IndexOf(stmp);
      if idx >= 0 then
        sret := sret + Chr(255) + Chr((dict2.Objects[idx] as TNumber).value)
      else
        sret := sret + stmp;
    end;
    stmp := '';
  end;

begin
  sret := '';
  stmp := '';
  for i := 1 to Length(s) do
    if isAlfaNumeric(s[i]) then
      stmp := stmp + s[i]
    else
    begin
      addtoret;
      sret := sret + s[i];
    end;
  addtoret;
  Result := sret;
  for i := 1 to 22 do
    if Length(DC_dictionary_desc1[i].aliasword) > 1 then
      Result := StringReplace(Result, DC_dictionary_desc1[i].aliasword, Chr(i), [rfReplaceAll]);
  len := Length(Result);
  for i := 0 to DC_NUM_REPLACE_STR - 1 do
  begin
    len2 := DC_Replace[i].len;
    if len2 > 3 then
      if len > len2 then
        Result := StringReplace(Result, DC_Replace[i].aliasword, DC_Replace[i].wordcode, []);
  end;
end;

function BI_DecodeDesc(const s: string): string;
var
  i: integer;
  id: integer;
begin
  Result := '';
  id := 1;
  for i := 1 to Length(s) do
  begin
    if id = 3 then
    begin
      if Ord(s[i]) < DC_NUM_REPLACE_STR then
        Result := Result + DC_Replace[Ord(s[i])].aliasword
      else
        Result := Result + s[i];
      id := 1;
    end
    else if id = 2 then
    begin
      if s[i] = Chr(255) then
        id := 3
      else
      begin
        Result := Result + DC_dictionary_desc2[Ord(s[i])].aliasword;
        id := 1;
      end;
    end
    else if s[i] = Chr(255) then
      id := 2
    else
      Result := Result + DC_dictionary_desc1[Ord(s[i])].aliasword;
  end;
end;

procedure InitDict(var dict: TStringList; const DC_dictionary_desc: DC_Dictionary_p);
var
  i: integer;
begin
  dict := TStringList.Create;
  dict.CaseSensitive := True;
  for i := 0 to 255 do
    if Length(DC_dictionary_desc[i].aliasword) > 1 then
      dict.AddObject(DC_dictionary_desc[i].aliasword, TNumber.Create(i));
  dict.Sorted := True;
end;

procedure ShutDownDict(var dict: TStringList);
var
  i: integer;
begin
  for i := 0 to dict.Count - 1 do
    dict.Objects[i].Free;
  dict.Free;
end;

procedure InitReplace;
var
  i: integer;
begin
  for i := 0 to DC_NUM_REPLACE_STR - 1 do
    DC_Replace[i].len := Length(DC_Replace[i].aliasword);
end;

initialization
  InitDict(dict1, @DC_dictionary_desc1);
  InitDict(dict2, @DC_dictionary_desc2);
  InitReplace;

finalization
  ShutDownDict(dict1);
  ShutDownDict(dict2);

end.
