----------------------------------------------
test: tests/ttf/ttf.sh_it | tests/ttf/envy.ttf
----------------------------------------------

f: font = {
  off_table = {
    sfnf_ver = [
      0 = h 0 ,
      1 = h 1 ,
      2 = h 0 ,
      3 = h 0
    ] ,
    num_tables = 15 ,
    searchRange = 128 ,
    entrySelector = 3 ,
    range_shift = 112
  } ,
  table_directory = [
    0 = { tag = OS/2 , check_sum = 3879676840 , offset = 376 , length = 96 , table_data = [ 0 , 3 , 4 , 76 , 1 , 144 , 0 , 5 , 0 , 0 ] } ,
    1 = { tag = VDMX , check_sum = 1852667374 , offset = 3060 , length = 1504 , table_data = [ 0 , 0 , 0 , 1 , 0 , 1 , 1 , 1 , 1 , 1 ] } ,
    2 = { tag = cmap , check_sum = 292165991 , offset = 4564 , length = 4570 , table_data = {
        version = 0 ,
        num_tables = 3 ,
        encoding_tables = [
          0 = {
            plat_id = 0 ,
            encode_id = 3 ,
            offset_sub = 2560 ,
            dat = {
              format = 4 ,
              length = 2010 ,
              lang = 0 ,
              format_data = {
                seg2 = 240 ,
                searchRange = 128 ,
                entrySel = 6 ,
                range_shift = 112 ,
                endCode = [ 126 , 383 , 402 , 468 , 489 , 505 , 511 , 539 , 551 , 563 ] ,
                reserve = 0 ,
                startCode = [ 32 , 160 , 401 , 463 , 486 , 504 , 508 , 536 , 550 , 562 ] ,
                idDelta = [ 0 , 0 , 0 , 26 , 0 , 65529 , 0 , 0 , 65487 , 65477 ] ,
                idRangeOffset = [ 240 , 428 , 874 , 0 , 874 , 0 , 878 , 884 , 0 , 0 ] ,
                glypharr = [ ]
              }
            }
          } ,
          1 = {
            plat_id = 1 ,
            encode_id = 0 ,
            offset_sub = 28 ,
            dat = {
              format = 6 ,
              length = 522 ,
              lang = 0 ,
              format_data = [ 0 , 0 , 1 , 0 , 0 , 1 , 0 , 0 , 0 , 0 ]
            }
          } ,
          2 = {
            plat_id = 3 ,
            encode_id = 1 ,
            offset_sub = 550 ,
            dat = {
              format = 4 ,
              length = 2010 ,
              lang = 0 ,
              format_data = {
                seg2 = 240 ,
                searchRange = 128 ,
                entrySel = 6 ,
                range_shift = 112 ,
                endCode = [ 126 , 383 , 402 , 468 , 489 , 505 , 511 , 539 , 551 , 563 ] ,
                reserve = 0 ,
                startCode = [ 32 , 160 , 401 , 463 , 486 , 504 , 508 , 536 , 550 , 562 ] ,
                idDelta = [ 0 , 0 , 0 , 26 , 0 , 65529 , 0 , 0 , 65487 , 65477 ] ,
                idRangeOffset = [ 240 , 428 , 874 , 0 , 874 , 0 , 878 , 884 , 0 , 0 ] ,
                glypharr = [ ]
              }
            }
          }
        ]
      } } ,
    3 = { tag = cvt  , check_sum = 53285024 , offset = 9640 , length = 38 , table_data = [ 0 , 39 , 0 , 158 , 1 , 60 , 0 , 158 , 1 , 60 ] } ,
    4 = { tag = fpgm , check_sum = 106273847 , offset = 9136 , length = 371 , table_data = [ 184 , 0 , 0 , 44 , 75 , 184 , 0 , 9 , 80 , 88 ] } ,
    5 = { tag = gasp , check_sum = 1507338 , offset = 89908 , length = 16 , table_data = [ 0 , 0 , 0 , 3 , 0 , 8 , 0 , 2 , 0 , 16 ] } ,
    6 = { tag = glyf , check_sum = 2910639616 , offset = 9680 , length = 72668 , table_data = [ 255 , 255 , 0 , 158 , 0 , 0 , 3 , 188 , 6 , 202 ] } ,
    7 = { tag = head , check_sum = 3937900446 , offset = 252 , length = 54 , table_data = [ 0 , 1 , 0 , 0 , 0 , 0 , 20 , 57 , 255 , 19 ] } ,
    8 = { tag = hhea , check_sum = 196674804 , offset = 308 , length = 36 , table_data = [ 0 , 1 , 0 , 0 , 7 , 104 , 254 , 38 , 0 , 0 ] } ,
    9 = { tag = hmtx , check_sum = 3620371100 , offset = 472 , length = 2588 , table_data = [ 4 , 76 , 0 , 0 , 0 , 0 , 0 , 0 , 4 , 76 ] } ,
    10 = { tag = loca , check_sum = 2362954746 , offset = 82348 , length = 1296 , table_data = [ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ] } ,
    11 = { tag = maxp , check_sum = 79561398 , offset = 344 , length = 32 , table_data = [ 0 , 1 , 0 , 0 , 2 , 135 , 0 , 224 , 0 , 36 ] } ,
    12 = { tag = name , check_sum = 2814078438 , offset = 83644 , length = 1484 , table_data = {
        format = 0 ,
        count = 27 ,
        offset = 330 ,
        name_records = [
          0 = { 1 , 0 , 0 , 0 , 74 , 0 , Copyright (c) 2006-2008 by Envy Technologies Limited. All rights reserved. } ,
          1 = { 1 , 0 , 0 , 1 , 11 , 74 , Envy Code R } ,
          2 = { 1 , 0 , 0 , 2 , 7 , 85 , Regular } ,
          3 = { 1 , 0 , 0 , 3 , 42 , 92 , EnvyTechnologiesLimited: Envy Code R: 2006 } ,
          4 = { 1 , 0 , 0 , 4 , 11 , 134 , Envy Code R } ,
          5 = { 1 , 0 , 0 , 5 , 13 , 145 , Version 0.079 } ,
          6 = { 1 , 0 , 0 , 6 , 9 , 158 , EnvyCodeR } ,
          7 = { 1 , 0 , 0 , 7 , 56 , 167 , Envy Code R is a trademark of Envy Technologies Limited. } ,
          8 = { 1 , 0 , 0 , 8 , 25 , 223 , Envy Technologies Limited } ,
          9 = { 1 , 0 , 0 , 9 , 12 , 248 , Damien Guard } ,
          10 = { 1 , 0 , 0 , 10 , 74 , 260 , Copyright (c) 2006-2008 by Envy Technologies Limited. All rights reserved. } ,
          11 = { 1 , 0 , 0 , 11 , 25 , 334 , http://www.envytech.co.uk } ,
          12 = { 1 , 0 , 0 , 12 , 22 , 359 , http://www.damieng.com } ,
          13 = { 1 , 0 , 0 , 18 , 11 , 381 , Envy Code R } ,
          14 = { 3 , 1 , 1033 , 0 , 148 , 392 , [ ] } ,
          15 = { 3 , 1 , 1033 , 1 , 22 , 540 , [ ] } ,
          16 = { 3 , 1 , 1033 , 2 , 14 , 562 , [ ] } ,
          17 = { 3 , 1 , 1033 , 3 , 84 , 576 , [ ] } ,
          18 = { 3 , 1 , 1033 , 4 , 22 , 660 , [ ] } ,
          19 = { 3 , 1 , 1033 , 5 , 26 , 682 , [ ] } ,
          20 = { 3 , 1 , 1033 , 6 , 18 , 708 , [ ] } ,
          21 = { 3 , 1 , 1033 , 7 , 112 , 726 , [ ] } ,
          22 = { 3 , 1 , 1033 , 8 , 50 , 838 , [ ] } ,
          23 = { 3 , 1 , 1033 , 9 , 24 , 888 , [ ] } ,
          24 = { 3 , 1 , 1033 , 10 , 148 , 912 , [ ] } ,
          25 = { 3 , 1 , 1033 , 11 , 50 , 1060 , [ ] } ,
          26 = { 3 , 1 , 1033 , 12 , 44 , 1110 , [ ] }
        ]
      } } ,
    13 = { tag = post , check_sum = 2719548887 , offset = 85128 , length = 4779 , table_data = [ 0 , 2 , 0 , 0 , 0 , 0 , 0 , 0 , 255 , 51 ] } ,
    14 = { tag = prep , check_sum = 2406111828 , offset = 9508 , length = 130 , table_data = [ 184 , 0 , 0 , 43 , 0 , 186 , 0 , 1 , 0 , 2 ] }
  ]
}
