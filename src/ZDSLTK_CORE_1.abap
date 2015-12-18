**********************************************************************
* Include ZDSLTK_CORE_1
*
* DSL toolkit, core, version 1
*
* Some helper classes for writing DSL parsers in ABAP
**********************************************************************
* Creation date   : 2015-12-16
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


"! Common type definitions
INTERFACE lif_dsltk_types.

  TYPES:
    mvt_token_id   TYPE i,
    mvt_token_text TYPE string,

    BEGIN OF mst_code_position,
      line   TYPE i,
      column TYPE i,
    END OF mst_code_position.

ENDINTERFACE.


"! Exception class used throughout the parser
CLASS lcx_parsing DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.

    "! Constructor
    "! @parameter iv_token_type | Token type as string
    "! @parameter iv_line       | The line of the input which caused the exception
    "! @parameter iv_column     | The column of the input which caused the exception
    METHODS  constructor IMPORTING iv_token_text TYPE lif_dsltk_types=>mvt_token_text OPTIONAL
                                   is_code_pos   TYPE lif_dsltk_types=>mst_code_position OPTIONAL.

    DATA: mv_token_text READ-ONLY   TYPE lif_dsltk_types=>mvt_token_text,
          ms_code_pos   READ-ONLY   TYPE lif_dsltk_types=>mst_code_position.
ENDCLASS.

CLASS lcx_parsing IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_token_text   = iv_token_text.
    ms_code_pos     = is_code_pos.
  ENDMETHOD.
ENDCLASS.


"! Super-class for parse tree nodes
CLASS lcl_node DEFINITION.
  PUBLIC SECTION.

    TYPES: mtt_children   TYPE STANDARD TABLE OF REF TO lcl_node WITH NON-UNIQUE DEFAULT KEY.

    METHODS:
      "! Constructor
      "! @parameter iv_token_id | Token ID
      "! @parameter iv_token    | Token string
      "! @parameter is_code_pos | Start of the token in the source code
      constructor IMPORTING iv_token_id TYPE lif_dsltk_types=>mvt_token_id
                            iv_token    TYPE string OPTIONAL
                            is_code_pos TYPE lif_dsltk_types=>mst_code_position OPTIONAL,

      "! Appends a child to the node's list of children
      "! @parameter io_node | The child which shall be added
      "! @parameter ro_node | The node itself (for method chaining)
      add_child   FINAL IMPORTING io_node        TYPE REF TO lcl_node
                        RETURNING VALUE(ro_node) TYPE REF TO lcl_node,

      "! Removes all children
      "! @parameter ro_node | The node itself (for method chaining)
      remove_children FINAL  RETURNING VALUE(ro_node) TYPE REF TO lcl_node.

    DATA: mv_token_id READ-ONLY TYPE lif_dsltk_types=>mvt_token_id,
          mv_token    READ-ONLY TYPE string,
          ms_code_pos READ-ONLY TYPE lif_dsltk_types=>mst_code_position,
          mt_children READ-ONLY TYPE mtt_children.

ENDCLASS.

CLASS lcl_node IMPLEMENTATION.

  METHOD constructor.
    mv_token_id   = iv_token_id.
    mv_token      = iv_token.
    ms_code_pos   = is_code_pos.
  ENDMETHOD.

  METHOD add_child.
    APPEND io_node TO mt_children.
    ro_node = me.
  ENDMETHOD.

  METHOD remove_children.
    CLEAR mt_children.
    ro_node = me.
  ENDMETHOD.

ENDCLASS.


"! Abstract super-class for DSL parsers
CLASS lcl_parser DEFINITION ABSTRACT.

  PUBLIC SECTION.

    TYPES: mtt_string_tab TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.

    METHODS:

      constructor,

      "! Parses the input and constructs a parse tree
      "! @parameter it_input            | The input to be parsed
      "! @parameter rt_error_messages   | Parsing was successful if and only if this table
      "!                                  is empty after return
      parse FINAL     IMPORTING it_input                 TYPE mtt_string_tab
                      RETURNING VALUE(rt_error_messages) TYPE mtt_string_tab,

      "! Returns the root node of the parse tree if parse() did not return
      "! any errors
      "! @parameter ro_node | Root node of the parse tree
      get_root_node FINAL RETURNING VALUE(ro_node) TYPE REF TO lcl_node.


  PROTECTED SECTION.

    TYPES: mrt_node TYPE REF TO lcl_node.

    METHODS:
        "! Hook methods invoked from parse()
        "! Has to be redefined in sub-class. Shall trigger the parsing process.
        "! @parameter ro_node   | The root node of the parse tree
        "! @raising lcx_parsing | If parsing didn't succeed this exception shall be raised
        parse_input ABSTRACT RETURNING VALUE(ro_node) TYPE mrt_node RAISING lcx_parsing,

        "! Factory method for nodes, to be implemented in each sub-class
        "! @parameter iv_token_id   | The token ID
        "! @parameter iv_token      | The token text
        "! @parameter is_code_pos   | The source code position of the token
        "! @parameter ro_node       | The created node object
        "! @raising lcx_parsing     | For notifying the parser about errors
        create_node ABSTRACT IMPORTING iv_token_id   TYPE lif_dsltk_types=>mvt_token_id
                                       iv_token      TYPE string OPTIONAL
                                       is_code_pos   TYPE lif_dsltk_types=>mst_code_position OPTIONAL
                             RETURNING VALUE(ro_node) TYPE mrt_node RAISING lcx_parsing.

    METHODS:
        "! Skips whitespace and newlines and reads a single token
        "! @parameter iv_regex      | The regular expression to use for matching a token. Enclose (a part of) the matched
        "!                          | string in parentheses to extract the part and assign it as token text to the
        "!                          | created token.
        "! @parameter iv_token_id   | The token ID that shall be assigned to the new token
        "! @parameter iv_token_text | The token text to be used in the raised exception if parsing didn't succeed.
        "!                            This is used only if iv_token_id is supplied. If no token ID is supplied and
        "!                            parsing failed, iv_regex is used as token text in the created exception.
        "! @parameter ro_token      | The parsed token
        "! @raising lcx_parsing     | If parsing the requested token wasn't possible
        read_token FINAL   IMPORTING   iv_regex          TYPE string
                                       iv_token_id       TYPE lif_dsltk_types=>mvt_token_id    OPTIONAL
                                       iv_token_text     TYPE lif_dsltk_types=>mvt_token_text  OPTIONAL
                           RETURNING VALUE(ro_token)     TYPE mrt_node RAISING lcx_parsing,

        "! Stores the current source code position on an internal stack.
        "! Use before trying to parse several alternatives.
        push_offsets       FINAL,

        "! Clears the internal stack of saved source code positions.
        "! Use after parsing one of several alternatives is finished and the stored
        "! source code position(s) are no longer needed.
        pop_offsets        FINAL,

        "! Restores the previous source code position.
        "! Use after failed attempt to parse one of several alternatives.
        reset_offsets      FINAL,

        "! Skip whitespace. The method name says it all.
        "!
        "! You probably don't need to use this method at all, since read_token implicitly
        "! skips whitespace before trying to read a token.
        skip_whitespace    FINAL,

        "! Return the current source code position
        "! @parameter rs_code_pos   | The current source code position
        current_pos        FINAL RETURNING VALUE(rs_code_pos) TYPE lif_dsltk_types=>mst_code_position.


  PRIVATE SECTION.

    TYPES: BEGIN OF mst_offsets,
             line   TYPE i,
             column TYPE i,
           END OF mst_offsets.

    DATA: ms_offsets          TYPE mst_offsets,
          mt_input            TYPE mtt_string_tab,
          mo_whitespace_regex TYPE REF TO cl_abap_regex,
          mt_offset_stack     TYPE STANDARD TABLE OF mst_offsets WITH NON-UNIQUE DEFAULT KEY,
          mo_root_node        TYPE REF TO lcl_node.

    METHODS:
      check_dangling_input  RAISING lcx_parsing.

ENDCLASS.


CLASS lcl_parser IMPLEMENTATION.

  METHOD current_pos.
    rs_code_pos = VALUE #( line     = ms_offsets-line + 1
                           column   = ms_offsets-column + 1 ).
  ENDMETHOD.

  METHOD get_root_node.
    ro_node = mo_root_node.
  ENDMETHOD.

  METHOD check_dangling_input.
    skip_whitespace( ).

    ASSIGN mt_input[ 1 + ms_offsets-line ] TO FIELD-SYMBOL(<lv_line>).
    IF ms_offsets-column < strlen( <lv_line> ).
      RAISE EXCEPTION TYPE lcx_parsing
        EXPORTING
          iv_token_text = `dangling input`
          is_code_pos   = current_pos( ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    mo_whitespace_regex = NEW cl_abap_regex( pattern = '^\s+' ).
  ENDMETHOD.

  METHOD push_offsets.
    APPEND INITIAL LINE TO mt_offset_stack ASSIGNING FIELD-SYMBOL(<ls_offsets>).
    <ls_offsets> = ms_offsets.
  ENDMETHOD.

  METHOD reset_offsets.
    ASSERT lines( mt_offset_stack ) > 0.
    ms_offsets =  mt_offset_stack[ lines( mt_offset_stack ) ].
    pop_offsets( ).
  ENDMETHOD.

  METHOD pop_offsets.
    DELETE mt_offset_stack INDEX lines( mt_offset_stack ).
  ENDMETHOD.

  METHOD parse.

    mt_input          = it_input.
    CLEAR ms_offsets.

    TRY.

        CLEAR mo_root_node.

        DATA(lo_parse_tree_root) = parse_input( ).

        check_dangling_input( ).

        mo_root_node = lo_parse_tree_root.

      CATCH lcx_parsing INTO DATA(lx_e).

        WHILE lx_e IS BOUND.
          APPEND |Line { lx_e->ms_code_pos-line }, column { lx_e->ms_code_pos-column }: Failed to parse { lx_e->mv_token_text }|
            TO rt_error_messages.

          lx_e ?= lx_e->previous.
        ENDWHILE.

    ENDTRY.

  ENDMETHOD.

  METHOD read_token.

    skip_whitespace( ).

    DATA(lv_regex) = `^` && iv_regex.

    " The following code copies the to be parsed substring of the current line
    " into lv_input.
    " Copying seems to be necessary since offsets cannot be used with strings.
    " I guess it could be prevented by switching from a table of strings as input
    " to a table of, say, char80.

    ASSIGN mt_input[ 1 + ms_offsets-line ] TO FIELD-SYMBOL(<lv_line>).
    DATA(lv_input_length) = strlen( <lv_line> ) - ms_offsets-column.
    DATA(lv_input)        = <lv_line>+ms_offsets-column(lv_input_length).

    " Find the keyword
    DATA(lo_regex)    = cl_regex_cache=>get_singleton( )->get_regex( pattern = lv_regex ).
    DATA(lo_matcher)  = lo_regex->create_matcher( text = lv_input ).

    IF lo_matcher->find_next( ).
      DATA(ls_match_result) = lo_matcher->get_match( ).
      DATA(lv_token) = lv_input+ls_match_result-offset(ls_match_result-length).
      ro_token = create_node(
               iv_token_id   = iv_token_id
               iv_token      = lv_token
               is_code_pos   = VALUE #( line   = ms_offsets-line + 1
                                        column = ms_offsets-column + 1 ) ).
      ms_offsets-column = ms_offsets-column + ls_match_result-offset + ls_match_result-length.
    ELSE.
      RAISE EXCEPTION TYPE lcx_parsing
        EXPORTING
          iv_token_text = COND #( WHEN iv_token_id IS INITIAL THEN iv_regex ELSE iv_token_text )
          is_code_pos   = current_pos( ).
    ENDIF.

  ENDMETHOD.

  METHOD skip_whitespace.

    ASSIGN mt_input[ 1 + ms_offsets-line ] TO FIELD-SYMBOL(<lv_line>).
    DATA(lv_input_length) = strlen( <lv_line> ) - ms_offsets-column.
    DATA(lv_input)        = <lv_line>+ms_offsets-column(lv_input_length).

    DATA(lo_ws_matcher)   = mo_whitespace_regex->create_matcher( text = lv_input ).

    IF lo_ws_matcher->find_next( ).
      ms_offsets-column = ms_offsets-column + lo_ws_matcher->get_length( ).
    ENDIF.

    " Go to new line, if needed
    IF ms_offsets-column >= strlen( <lv_line> ) AND ms_offsets-line + 1 < lines( mt_input ).
      ms_offsets-line    = ms_offsets-line + 1.
      ms_offsets-column  = 0.
      skip_whitespace( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.