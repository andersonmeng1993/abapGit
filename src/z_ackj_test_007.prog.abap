*&---------------------------------------------------------------------*
*& Modulpool         Z_ACKJ_TEST_007
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


INCLUDE Z_ACKJ_TEST_007_TOP                     .    " global Data

INITIALIZATION.

START-OF-SELECTION.


" TEMP - AB HIER LÖSCHEN
DATA: go_master TYPE REF TO /cdx/35cl_talk_engine,
      gt_pin_coll_kw TYPE /cdx/35talk_pin_coll_kw_t,
      gt_pin_coll TYPE /cdx/35talk_pin_coll_t.
" BIS HIERHER LÖSCHEN

" Bestellpositionen
TYPES:  BEGIN OF ty_pos,
  posidn      TYPE i,
  matnr       TYPE matnr,
  param02     TYPE /cdx/3_param02,
  page_start  TYPE /cdx/3_primeocr_page_no,
  page_end    TYPE /cdx/3_primeocr_page_no,
  line_y1     TYPE /cdx/3_primeocr_line_no,
  line_y2     TYPE /cdx/3_primeocr_line_no,
  word_y1     TYPE /cdx/3_primeocr_word_y1,
  word_y2     TYPE /cdx/3_primeocr_word_y2,
        END OF ty_pos.

" Variablen
DATA:  lo_matnr  TYPE REF TO /cdx/35cl_talk_matnr,
       lo_ebeln  TYPE REF TO /cdx/35cl_talk_ebeln,
       lo_datum  TYPE REF TO /cdx/35cl_talk_root_date,
       lt_pos   TYPE STANDARD TABLE OF ty_pos,
       lt_date  TYPE STANDARD TABLE OF /cdx/35talk_pin_coll,
       lt_temp  TYPE STANDARD TABLE OF /cdx/35talk_pin_coll,
       lt_eket  TYPE STANDARD TABLE OF /cdx/35dbi_eket,
       lt_ekpo  TYPE STANDARD TABLE OF /cdx/35dbi_ekpo,
       ls_pos   TYPE ty_pos,
       ls_eket  TYPE /cdx/35dbi_eket,
       ls_ekpo  TYPE /cdx/35dbi_ekpo,
       ls_kw    TYPE /cdx/35talk_pin_coll_kw,
       ls_matnr TYPE /cdx/35talk_pin_coll,
       ls_pin   TYPE /cdx/35talk_pin_coll,
       ls_datum TYPE /cdx/35talk_pin_coll,
       ls_tuple TYPE /cdx/35talk_tuple,
       lv_tabix TYPE sy-tabix,
       lv_iskw  TYPE abap_bool.

FIELD-SYMBOLS: <matnr> TYPE /cdx/35talk_pin_coll,
               <tuple> TYPE REF TO /cdx/35cl_srv_tuple.

" Vorbereitung
lo_matnr ?= go_master->oref_get( 'MATNR' ).
lo_ebeln ?= go_master->oref_get( 'EBELN' ).
lo_datum ?= go_master->oref_get( 'DATE' ).
ASSIGN go_master->go_srv_tuple TO <tuple>.

CHECK lo_matnr->gt_tag_coll IS NOT INITIAL.
CHECK lo_ebeln->gt_tag_coll IS NOT INITIAL.

" Kleine Positions-Tabelle aufbauen
LOOP AT lo_matnr->gt_pin_coll ASSIGNING <matnr>.
  CLEAR: ls_pos, ls_matnr.
  lv_tabix = sy-tabix + 1.

  " Obergrenze Y1
  ls_pos-matnr      = <matnr>-pin_value.
  ls_pos-line_y1    = <matnr>-line_no.
  ls_pos-word_y1    = <matnr>-word_y1.
  ls_pos-page_start = <matnr>-page_no.

  " Untergrenze Y2
  READ TABLE lo_matnr->gt_pin_coll INDEX lv_tabix TRANSPORTING NO FIELDS.

  " Wenn Folgeposition existiert Y1 davon nehmen
  IF sy-subrc = 0.
    READ TABLE lo_matnr->gt_pin_coll INDEX lv_tabix INTO ls_matnr.
    ls_pos-line_y2  = ls_matnr-line_no - 1.
    ls_pos-word_y2  = ls_matnr-word_y2.
    ls_pos-page_end = ls_matnr-page_no.
  ELSE.
    " Hole Zeile vor Endbeträgen auf aktueller Seite (Typ 4)
    READ TABLE <tuple>->gt_tuple INTO ls_tuple WITH KEY tupidn = '0004'
                                                        page_no = <matnr>-page_no. " ggf hier Seite anders bestimmen
    IF sy-subrc <> 4.
      READ TABLE <tuple>->gt_tuple INTO ls_tuple WITH KEY tupidn = '0004'.
    ENDIF.
    ls_pos-line_y2  = ls_tuple-line_x2 - 1.
    ls_pos-word_y2  = ls_tuple-word_y2.
    ls_pos-page_end = ls_tuple-page_no.
  ENDIF.
  APPEND ls_pos TO lt_pos.
ENDLOOP.

" KW-Tabelle aufbauen für spätere Suche
"CALL METHOD me->action_pin_built_kw_tab.

" srv_pin-Einträge löschen damit wir Platz haben
CALL METHOD go_master->go_srv_pin->delete_pin_by_conidn
  EXPORTING
    i_conidn = 'EINDT'.

" Einteilungsdaten holen
CLEAR: lt_eket, lt_ekpo.
APPEND LINES OF lo_ebeln->gt_eket TO lt_eket.
APPEND LINES OF lo_ebeln->gt_ekpo TO lt_ekpo.
APPEND LINES OF lo_datum->gt_pin_coll TO lt_date.

" Vorgehen:
" - Loop über jede Einteilung
" - Passende EBELP dazu holen
" - Prüfen, passende Position vorhanden mit MATNR
" - Wenn ja, alle Daten im Positionsgebiet der MATNR holen
" - PARAM02 ausfüllen und als Pin speichern

LOOP AT lt_eket INTO ls_eket.
  CLEAR: ls_ekpo, ls_pos, ls_pin.
  READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = ls_eket-ebeln
                                           ebelp = ls_eket-ebelp.
  IF sy-subrc <> 0.
    CONTINUE.
  ENDIF.

  " Bestellposition holen
  READ TABLE lt_pos INTO ls_pos WITH KEY matnr = ls_ekpo-matnr.
  IF sy-subrc <> 0.
    CONTINUE.
  ENDIF.

  " Toleranzen unnötig, da Datum nur im Rahmen der Positionen gesucht wird

  " KW-Pins aus dem jeweiligen Bereich
  LOOP AT gt_pin_coll_kw INTO ls_kw WHERE ( page_no GE ls_pos-page_start AND
                                            word_y1 GE ls_pos-word_y1 ) AND
                                          ( page_no LE ls_pos-page_end AND
                                            word_y2 LE ls_pos-word_y2 ).
    MOVE-CORRESPONDING ls_kw TO ls_pin.
    ls_pin-pin_conidn = 'EINDT'.
    CONCATENATE ls_eket-ebeln '\' ls_eket-ebelp '\' ls_eket-etenr INTO ls_pin-param02.
    APPEND ls_pin TO lt_temp.
  ENDLOOP.

  " Datum-Pins aus dem jeweiligen Bereich
  LOOP AT lt_date INTO ls_datum WHERE ( page_no GE ls_pos-page_start AND
                                          word_y1 GE ls_pos-word_y1 ) AND
                                        ( page_no LE ls_pos-page_end AND
                                          word_y2 LE ls_pos-word_y2 ).
    MOVE-CORRESPONDING ls_datum TO ls_pin.
    ls_pin-pin_conidn = 'EINDT'.
    CONCATENATE ls_eket-ebeln '\' ls_eket-ebelp '\' ls_eket-etenr INTO ls_pin-param02.
    APPEND ls_pin TO lt_temp.
  ENDLOOP.
ENDLOOP.

DELETE ADJACENT DUPLICATES FROM lt_temp.
APPEND LINES OF lt_temp TO gt_pin_coll.
