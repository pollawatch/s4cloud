CLASS ycl_apj DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  CLASS-METHODS create_job IMPORTING iv_do type I_OutboundDelivery-OutboundDelivery.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_apj IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
   "For Test via ADT
   ycl_apj=>create_job( iv_do = '008000031X'  ).
  ENDMETHOD.

  METHOD create_job.

DATA: lt_job_parameter TYPE cl_apj_rt_api=>tt_job_parameter_value.

    DATA lv_job_text         TYPE cl_apj_rt_api=>ty_job_text VALUE 'Demo Application Job'.

    DATA lv_template_name       TYPE cl_apj_rt_api=>ty_template_name.

    DATA ls_start_info       TYPE cl_apj_rt_api=>ty_start_info.

    DATA ls_end_info         TYPE cl_apj_rt_api=>ty_end_info.

    DATA lv_jobname             TYPE cl_apj_rt_api=>ty_jobname.
    DATA lv_jobcount            TYPE cl_apj_rt_api=>ty_jobcount.

    DATA lv_status              TYPE cl_apj_rt_api=>ty_job_status.
    DATA lv_statustext          TYPE cl_apj_rt_api=>ty_job_status_text.

    DATA: txt TYPE string.
    DATA: tz  TYPE timezone.

    DATA dat TYPE d.
    DATA tim TYPE t.

    lv_template_name = 'YAPJ_TEMPLATE_DEMO'.
    lv_jobname = 'DEMO_APP_JOB_DO' && cl_abap_context_info=>get_system_date( ) && cl_abap_context_info=>get_system_time( ).

    " the immediate start would look like this:
*    ls_start_info-start_immediately = abap_true.

    dat = cl_abap_context_info=>get_system_date( ).
    tim = cl_abap_context_info=>get_system_time( ) + 5.

    tz = cl_abap_tstmp=>get_system_timezone(  ).

    CONVERT DATE dat TIME tim
            INTO TIME STAMP DATA(ts) TIME ZONE tz.

    ls_start_info-timestamp = ts.

    lt_job_parameter = VALUE #( ( name = 'P_DO'
                                t_value = VALUE #( ( sign = 'I' option = 'EQ' low = iv_do ) )
                                ) ).

    TRY.
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name   = lv_template_name
            iv_job_text            = lv_job_text
            is_start_info          = ls_start_info
            it_job_parameter_value = lt_job_parameter
*            iv_jobname             = lv_jobname
          IMPORTING
            ev_jobname             = lv_jobname
            ev_jobcount            = lv_jobcount
        ).


        cl_apj_rt_api=>get_job_status(
          EXPORTING
            iv_jobname         = lv_jobname
            iv_jobcount        = lv_jobcount
          IMPORTING
            ev_job_status      = lv_status
            ev_job_status_text = lv_statustext
        ).


*
      CATCH cx_apj_rt INTO DATA(exc).
.
    ENDTRY.



  ENDMETHOD.

ENDCLASS.
