**********************************************************************
* Report ZDSLTK_EXAMPLE_DATES
*
* Example for the DSL toolkit, core, version 1
*
* Parser for the grammar
*
* date-list = date-entry { "," date-entry }
* date-entry = date [ "-" date]
* date = "\d\d\d\d-\d\d?-\d\d?"
*
**********************************************************************
* Creation date   : 2015-12-17
* Author          : Valentin Huber
***********************************************************************
*
*   Copyright 2015 Valentin Huber
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*   you may not use this file except in compliance with the License.
*   You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*   distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*   See the License for the specific language governing permissions and
*   limitations under the License.
*
**********************************************************************
REPORT zdsltk_example_dates.


INCLUDE zdsltk_core_1.


CLASS lcl_dates_node DEFINITION INHERITING FROM lcl_node.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF gc_token_type,
                 date_list  TYPE lif_dsltk_types=>mvt_token_id VALUE 1,
                 date_entry TYPE lif_dsltk_types=>mvt_token_id VALUE 2,
                 date       TYPE lif_dsltk_types=>mvt_token_id VALUE 3,
               END OF gc_token_type.

ENDCLASS.


CLASS lcl_dates_parser DEFINITION INHERITING FROM lcl_parser.

  PROTECTED SECTION.

    METHODS:
      parse_input REDEFINITION,
      create_node REDEFINITION.

  PRIVATE SECTION.

    METHODS:
      read_comma  RAISING lcx_parsing,
      read_dash   RAISING lcx_parsing,
      read_date   RETURNING VALUE(ro_node) TYPE mrt_node RAISING lcx_parsing.

    METHODS:
      parse_date_list   RETURNING VALUE(ro_node) TYPE mrt_node RAISING lcx_parsing,
      parse_date_entry  RETURNING VALUE(ro_node) TYPE mrt_node RAISING lcx_parsing.

ENDCLASS.

CLASS lcl_dates_parser IMPLEMENTATION.

  METHOD parse_input.
    ro_node = parse_date_list( ).
  ENDMETHOD.

  METHOD create_node.
    ro_node = NEW lcl_dates_node(
        iv_token_id = iv_token_id
        iv_token    = iv_token
        is_code_pos = is_code_pos ).
  ENDMETHOD.

  METHOD read_comma.
    read_token( ',' ).
  ENDMETHOD.

  METHOD read_dash.
    read_token( '-' ).
  ENDMETHOD.

  METHOD read_date.
    ro_node = read_token(
              iv_regex      = '(\d\d\d\d-\d\d?-\d\d?)'
              iv_token_id   = lcl_dates_node=>gc_token_type-date
              iv_token_text = 'date' ).
  ENDMETHOD.

  METHOD parse_date_list.

    " Production rule:
    " date-list = date-entry { "," date-entry }

    ro_node = create_node( lcl_dates_node=>gc_token_type-date_list ).

    DATA(lo_date_entry) = parse_date_entry( ).
    ro_node->add_child( lo_date_entry ).

    DO.
      TRY.
          push_offsets( ).
          read_comma( ).
          lo_date_entry = parse_date_entry( ).
          ro_node->add_child( lo_date_entry ).
          pop_offsets( ).
        CATCH lcx_parsing.
          reset_offsets( ).
          EXIT.
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD parse_date_entry.

    " Production rule:
    " date-entry = date [ "-" date]

    ro_node = create_node( lcl_dates_node=>gc_token_type-date_entry ).

    DATA(lo_date_1) = read_date( ).

    ro_node->add_child( lo_date_1 ).

    TRY.
        push_offsets( ).
        read_dash( ).
        DATA(lo_date_2) = read_date( ).
        ro_node->add_child( lo_date_2 ).
        pop_offsets( ).
      CATCH lcx_parsing.
        reset_offsets( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA(go_parser) = NEW lcl_dates_parser( ).

  BREAK-POINT.

  DATA(lt_error_messages) = go_parser->parse( it_input = VALUE #(
      ( ` 2015-01-01 - 2015-01-15, 2015-01-20, 2015-10-10 ` )
      ( `      - 2015-10-12  ` )
  ) ).

  LOOP AT lt_error_messages INTO DATA(ls_msg).
    WRITE: /, ls_msg.
  ENDLOOP.

  BREAK-POINT.