CLASS ycl_apj_logic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .
    INTERFACES if_oo_adt_classrun.
    METHODS api_update_item_text IMPORTING iv_do   TYPE I_OutboundDelivery-OutboundDelivery
                                           iv_item TYPE I_OutboundDeliveryItem-OutboundDeliveryItem
                                           iv_text TYPE string.

    METHODS api_create_item_text IMPORTING iv_do   TYPE I_OutboundDelivery-OutboundDelivery
                                           iv_item TYPE I_OutboundDeliveryItem-OutboundDeliveryItem
                                           iv_text TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_apj_logic IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.

    et_parameter_def = VALUE #( ( selname = 'P_DO' kind = if_apj_dt_exec_object=>parameter datatype = 'C' length = '10' param_text = 'Delivery Doucment' changeable_ind = abap_true ) ).

  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.


    DATA: lv_delivery_doc TYPE I_DeliveryDocument-DeliveryDocument.


    READ TABLE it_parameters INTO DATA(ls_parameter) WITH KEY selname = 'P_DO'.

    IF sy-subrc = 0.
      lv_delivery_doc = ls_parameter-low.
      READ ENTITIES OF i_outbounddeliverytp
        ENTITY outbounddelivery     BY \_item
        ALL FIELDS WITH VALUE #( ( OutboundDelivery = lv_delivery_doc ) )
        RESULT DATA(lt_do_item)
        REPORTED DATA(ls_msg_err)
        FAILED DATA(ls_status).

      IF ls_status-outbounddeliveryitem IS INITIAL.
        MODIFY ENTITIES OF I_OutboundDeliveryTP
           ENTITY OutboundDeliveryItem
           UPDATE FIELDS ( YY1_FD_ManuSerialNum_DLI )
           WITH VALUE #( FOR ls_do_item IN lt_do_item
                        (   %tky-OutboundDelivery = ls_do_item-%tky-OutboundDelivery
                            %tky-OutboundDeliveryItem = ls_do_item-%tky-OutboundDeliveryItem
                            YY1_FD_ManuSerialNum_DLI = 'SerialJob'
                            %control-YY1_FD_ManuSerialNum_DLI = '01' )
                       )
          REPORTED DATA(ls_msg_upd)
          FAILED   DATA(ls_status_upd)
          MAPPED   DATA(ls_mapped_upd).


        IF ls_status_upd-outbounddeliveryitem IS INITIAL.
          COMMIT ENTITIES.
        ELSE.
          RAISE EXCEPTION TYPE cx_apj_rt_content.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE cx_apj_rt_content.
      ENDIF.

    ELSE.
      "MESSAGE ID '38' TYPE 'S' NUMBER '000'  WITH 'Paramenter no found' INTO DATA(lv_msg).
      RAISE EXCEPTION TYPE cx_apj_rt_content.


    ENDIF.

  ENDMETHOD.
  METHOD if_oo_adt_classrun~main.
    DATA(lo_apj) = NEW ycl_apj_logic(  ).

*    lo_apj->api_update_item_text(
*      iv_do   = '80000316'
*      iv_item = '10'
*      iv_text = 'Test from adt'
*    ).

    lo_apj->api_create_item_text(
         iv_do   = '80000317'
         iv_item = '10'
         iv_text = 'Test serial from adt'
    ).


  ENDMETHOD.

  METHOD api_update_item_text.

    DATA:
      ls_business_data TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
      ls_entity_key    TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
      lo_http_client   TYPE REF TO if_web_http_client,
      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
      lo_request       TYPE REF TO /iwbep/if_cp_request_update,
      lo_response      TYPE REF TO /iwbep/if_cp_response_update.



    TRY.
        " Create http client
        TRY.
            DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                         comm_scenario  = 'YY1_DO_UPDATE'
                                                         comm_system_id = 'INT_HTTP'
                                                         service_id     = 'YY1_DO_API_REST'
            ).
          CATCH cx_http_dest_provider_error.
            "handle exception
            RETURN.
        ENDTRY.
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).


        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING
             is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                 proxy_model_id      = 'YCL_SCM_OUTBOUNDDELIVERY'
                                                 proxy_model_version = '0001' )
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/API_OUTBOUND_DELIVERY_SRV;v=2' ).

        ASSERT lo_http_client IS BOUND.


        " Set entity key
        ls_entity_key = VALUE #(
                  delivery_document       = iv_do
                  delivery_document_item  = iv_item
                  text_element            = '0001'
                  language                = 'EN' ).

        " Prepare the business data
        ls_business_data = VALUE #(
                  "delivery_document           = 'DeliveryDocument'
                  "delivery_document_item      = 'DeliveryDocumentItem'
                  "text_element                = 'TextElement'
                  "language                    = 'Language'
                  "text_element_description    = 'TextElementDescription'
                  text_element_text           = iv_text
                  "delivery_long_text_is_form  = abap_true
                   ).

        " Navigate to the resource and create a request for the update operation
        lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).

        lo_request = lo_resource->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-patch ).

        DATA lt_path TYPE /iwbep/if_cp_runtime_types=>ty_t_property_path .
        APPEND 'TEXT_ELEMENT_TEXT ' TO lt_path.
        lo_request->set_business_data( is_business_data = ls_business_data
                                       it_provided_property = lt_path ).



        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).

        " Get updated entity
*CLEAR ls_business_data.
*lo_response->get_business_data( importing es_business_data = ls_business_data ).

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection

      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        " Handle Exception
        RAISE SHORTDUMP lx_web_http_client_error.


    ENDTRY.
  ENDMETHOD.

  METHOD api_create_item_text.

    DATA:
      ls_business_data TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
      lo_http_client   TYPE REF TO if_web_http_client,
      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request       TYPE REF TO /iwbep/if_cp_request_create,
      lo_response      TYPE REF TO /iwbep/if_cp_response_create,
      lv_do            TYPE I_OutboundDelivery-OutboundDelivery,
      lv_item          TYPE I_OutboundDeliveryItem-OutboundDeliveryItem.
    DATA lt_partial_column TYPE /iwbep/if_cp_runtime_types=>ty_t_property_path .
    TRY.
        " Create http client
        TRY.
            DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                         comm_scenario  = 'YY1_DO_UPDATE'
                                                         comm_system_id = 'INT_HTTP'
                                                         service_id     = 'YY1_DO_API_REST'
            ).
          CATCH cx_http_dest_provider_error.
            "handle exception
            RETURN.
        ENDTRY.
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING
             is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                 proxy_model_id      = 'YCL_SCM_OUTBOUNDDELIVERY'
                                                 proxy_model_version = '0001' )
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/SAP/API_OUTBOUND_DELIVERY_SRV;v=2' ).

        ASSERT lo_http_client IS BOUND.


* Prepare business data
        lv_do = |{ iv_do ALPHA = IN }|.
        lv_item = |{ iv_item ALPHA = IN }|.
        ls_business_data = VALUE #(
                  delivery_document           = lv_do
                  delivery_document_item      = lv_item
                  text_element                = 'Z001'
                  language                    = 'EN'
*                  text_element_description    = 'Serial Number'
                  text_element_text           = iv_text
*                  delivery_long_text_is_form  = abap_false
                  ).

        " Navigate to the resource and create a request for the create operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->create_request_for_create( ).

        " Set the business data for the created entity

        APPEND 'DELIVERY_DOCUMENT' TO lt_partial_column.
        APPEND 'DELIVERY_DOCUMENT_ITEM' TO lt_partial_column.
        APPEND 'TEXT_ELEMENT' TO lt_partial_column.
        APPEND 'LANGUAGE' TO lt_partial_column.
        APPEND 'TEXT_ELEMENT_TEXT' TO lt_partial_column.


        lo_request->set_business_data( is_business_data = ls_business_data
                                       it_provided_property = lt_partial_column ).

        " Execute the request
        lo_response = lo_request->execute( ).

        " Get the after image
*lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection


      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        " Handle Exception

      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        " Handle Exception
        RAISE SHORTDUMP lx_web_http_client_error.

    ENDTRY.
  ENDMETHOD.

ENDCLASS.
