
IMAE_CENEU <- c("CE.DI.SA.", "CENEPA", "CANIMEL", "SEDIC", "INU", "URUGUAYANA")

IMAE_NEPHROS <- c("NEPHROS", "UDIR", "SANEF")

IMAE_DIAVERUM <- c("CETER", "COMECA", "CRANI 33", "CRANI COSTA DE ORO", 
                   "CRANI MINAS", "INTIR", "RENIS", "SEINE", "SENNIAD", "UNEDI",
                   "SENNIAD HEMO")

IMAE_PUBLICO <- c("HOSPITAL MACIEL", "HOSPITAL DE CLINICAS")

IMAE_PRIVADO <- c("ASOCIACION ESPAÑOLA", "SMI - SERVICIO MEDICO INTEGRAL",
                  "S.M.Q. SALTO", "COMEPA", "COMEF", "CASMU", "CASA DE GALICIA",
                  "UNIVERSAL", "A.M. DE SAN JOSE", "CAMEC", "CAMEDUR-CENICA", "CAMOC", 
                  "CANMU", "CENDIME", "COMEF", "COMEPA", "COMERO",
                  "GREMEDA", "HOSPITAL BRITANICO", "HOSPITAL EVANGELICO",
                  "SANATORIO AMERICANO", "HOSPITAL ITALIANO")

IMAE_SARI <- "SARI"

INST_ASSE <- c("HOSPITAL DE CLINICAS", "H.MACIEL", "ASSE GRATUITO", "ASSE-PREPAGO", 
               "H.PEREIRA ROSSELL", "H.SAINT BOIS", "HOSPITAL ESPAÑOL", "C.D. CANELONES", 
               "C.A. LAS PIEDRAS", "C.A. PANDO", "C.A. DE CIUDAD DEL PLATA",
               "C.A. CIUDAD DE LA COSTA", "R.A.P. (S.A.E.)", "H.VILARDEBO", 
               "C.A. RINCON DE LA BOLSA", "C.A.DIARIOS Y REVISTAS", "C.D. RIVERA", 
               "C.D. TREINTA Y TRES", "H.PASTEUR", "C.D. FLORIDA")

INST_IAMCIAMPP <- c("ASOCIACION ESPAÑOLA", "CASMU - IAMPP", "CASA DE GALICIA", "CIRCULO CATOLICO",
                    "EVANGELICO","IMPASA", "MUCAM", "SMI - SERVICIO MEDICO INTEGRAL", "GREMCA", 
                    "SISTEMA NOTARIAL DE SALUD*", "COMEF IAMPP", "UNIVERSAL", "COSEM IAMPP", "CUDAM", 
                    "CAAMEPA IAMPP", "COMUE *", "CRAMI IAMPP", "AMECOM IAMPP", "CAMEC IAMPP", 
                    "AMEDRIN IAMPP", "COMTA IAMPP", "AFITYC***", "CRAME IAMPP", "S.M.Q. SALTO", "MUCAM")

INST_CORPORATIVO <- c("H.POLICIAL", "ANCAP***", "ANV (BHU)***")

INST_SEGUROPRIVADO <- c("COPAMHI MEDICARE", "CIMA ESPAÑA*", "MP MEDICINA PERSONALIZADA",
                        "I.Q. SUDAMERICANO", "HOSPITAL BRITANICO", "SUMMUM FONASA", "BLUE CROSS FONASA")

PAC_INACTIVO <- c("Jubilado", "Menor dependiente (< 15 años)", "Sin ocupación")

PAC_ACTIVO <- c("Artesano", "Empleado comercio", "Empleado rural", "Empleado servicio", 
                "Estudiante", "Funcionario público", "Otros", "Profesional liberal",
                "Propietario agricultura", "Propietario de comercio", "Propietario industrial",
                "Propietario servicio", "Tareas del hogar")

# INGRESOS_HD ----
INGRESOS <- 
  read_sav("C:/Users/julie/OneDrive/Documentos/Proyecto Tesis/Databases/INGRESOS HD.sav") %>% 
  #filter(ZCASDEPAR=="MONTEVIDEO") %>%
  select(-c(
    APNEFROD0, APNEFROD1,
    APCARDD0, APCARDD1, APCARDD2, APCARDD3, APCARDD4,
    APRESPD0, APRESPD1,
    APNEURD0, APNEURD1, APNEURD2,
    APDIGED0, APDIGED1,
    APINMUD0, APINMUD1,
    APEMETD0, APEMETD1,
    APOTROD0, APOTROD1, APOTROD2,
    APHEMAD0, APHEMAD1,
    APNEOPD, ZAPNEOPD,
    SCEFPYM0, SCEFPYM1,
    SCEFBF0, SCEFBF1,
    
  )) %>% 
  rename(
    ante=APERSONA,
    
    ante_nefro=APNEFRO,
    ante_nefro_str0=ZAPNEFROD0,
    ante_nefro_str1=ZAPNEFROD1,
    ante_nefro_irc_fecha=APIRCF,
    ante_nefro_1control_fecha=APPCNFEC,
    ante_nefro_creatinina=APPCNCRE,
    
    ante_cardio=APCARD,
    ante_cardio_str0=ZAPCARDD0,
    ante_cardio_str1=ZAPCARDD1,
    ante_cardio_str2=ZAPCARDD2,
    ante_cardio_str3=ZAPCARDD3,
    ante_cardio_str4=ZAPCARDD4,
    
    ante_respi=APRESP,
    ante_respi_str0=ZAPRESPD0,
    ante_respi_str1=ZAPRESPD1,
    
    ante_neuro=APNEUR,
    ante_neuro_str0=ZAPNEURD0,
    ante_neuro_str1=ZAPNEURD1,
    ante_neuro_str2=ZAPNEURD2,
    
    ante_dige=APDIGE,
    ante_dige_str0=ZAPDIGED0,
    ante_dige_str1=ZAPDIGED1,
    
    ante_inmu=APINMU,
    ante_inmu_str0=ZAPINMUD0,
    ante_inmu_str1=ZAPINMUD1,
    
    ante_meta=APEMET,
    ante_meta_str0=ZAPEMETD0,
    ante_meta_str1=ZAPEMETD1,
    
    ante_otro=APOTRO,
    ante_otro_str0=ZAPOTROD0, 
    ante_otro_str1=ZAPOTROD1,
    ante_otro_str2=ZAPOTROD2,
    
    ante_hema=APHEMA,
    ante_hema_str0=ZAPHEMAD0, 
    ante_hema_str1=ZAPHEMAD1,
    
    ante_neo=APNEOP,
    ante_neo_str0=APNEOPDE,
    
    descom=SCDESU,
    descom_fecha=SCDESUF,
    coord=SCINGC,
    coord_fecha=SCINGF,
    coord_str=SCINGOB,
    
    exa_concien=SCEFCON,
    exa_piel0=ZSCEFPYM0,
    exa_piel1=ZSCEFPYM1,
    exa_nutri=SCEFVN,
    exa_buco0=ZSCEFBF0,
    exa_buco1=ZSCEFBF1,
    exa_linfo=SCEFLG,
    
    exa_cardio=SCEFCV,
    exa_cardio_ritmo=SCEFCVR,
    exa_cardio_frec=SCEFCVF,
    exa_cardio_soplos=SCEFCVS,
    exa_cardio_galope=SCEFCVG,
    exa_cardio_roces=SCEFCVO,
    exa_cardio_pulsosperi=SCEFCVP,
    exa_cardio_varices=SCEFCVV,
    exa_cardio_trombo=SCEFCVT,
    exa_cardio_presion=SCEFCVA,
    exa_cardio_pad=SCEFCVD,
    
    exa_pleuro=SCEFPP,
    exa_abdo=SCEFAB,
    exa_nervio=SCEFSN,
    exa_osteo=SCEFOA,
    exa_peso=SCEFPE,
    exa_altura=SCEFTA,
    exa_capacidad=SCDCF,
    
    med=EMEDIC,
    med_epo=EMERI,
    med_hierro_vo=EMHIEVO,
    med_hierro_iv=EMHIEIV,
    med_ieca=EMIECA,
    med_araii=EMARAII,
    med_esta=EMESTA,
    med_antia_pla=EMAPLA,
    med_calcio=EMCAL,
    med_capto_fos=EMCFOS,
    med_vitd=EMVITD,
    
    estu_azoemia=EAZOV,
    estu_azoemia_fecha=EAZOF,
    estu_creatinemia=ECREAV,
    estu_creatinemia_fecha=ECREAF,
    estu_cicreatina=ECIV,
    estu_cicreatina_fecha=ECIF,
    estu_iono_na=EIONONA,
    estu_iono_k=EIONOK,
    estu_iono_ca=EIONOCA,
    estu_iono_fecha=EIONOF,
    
    estu_hemo_hematocrito=EHEMOHTO,
    estu_hemo_hemoglob=EHEMOHB,
    estu_hemo_globlancos=EHEMOGB,
    estu_hemo_plaquetas=EHEMOPLA,
    estu_hemo_fecha=EHEMOF,
  ) %>% 
  select(-c(EFHEPA, EFHEPAF, EFHBILT, EFHFOSA, EFHPROT, EFHALBU, EEHPA, EEHPAF, 
            EEHTGO, EEHTGP, EMFERR, EMFERRF, EMFSIDE, EMFTRAN, EMFFERRI, 
            ELIPIDO, ELIPIDOF, ELCOLE, ELTRIG, ELHDL, ELLDL, EPTH, EPTHF, EPTHV, 
            EHBA1C, EHBA1CF, EHBA1CV)) %>% 
  mutate(
    ZCAIMAE=if_else(ZCAIMAE=="HOSPITAL ITALIANO", "UNIVERSAL", ZCAIMAE),
    CAIMAE=if_else(CAIMAE==63, 95, CAIMAE),
    ZCASINST=if_else(ZCASINST=="HOSPITAL ITALIANO", "UNIVERSAL", ZCASINST),
    chain=
      case_when(ZCAIMAE %in% IMAE_DIAVERUM ~ "DIAVERUM",
                ZCAIMAE %in% IMAE_CENEU ~ "CENEU",
                ZCAIMAE %in% IMAE_NEPHROS ~ "NEPHROS",
                ZCAIMAE %in% IMAE_SARI ~ "SARI",
                ZCAIMAE %in% IMAE_PUBLICO ~ "PUBLICO",
                ZCAIMAE %in% IMAE_PRIVADO ~ "PRIVADO",
                TRUE ~ NA),
    tipo_inst=
      case_when(ZCASINST %in% INST_ASSE ~ "ASSE",
                ZCASINST %in% INST_IAMCIAMPP ~ "IAMC/IAMPP",
                ZCASINST %in% INST_CORPORATIVO ~ "CORPORATIVO",
                ZCASINST %in% INST_SEGUROPRIVADO ~ "SEGURO PRIVADO",
                TRUE ~ NA),
    ZCASINST=if_else(tipo_inst=="ASSE", "ASSE", ZCASINST), #RENOMBRO INSTITUCION A ASSE
    tipo_imae=
      case_when(ZCAIMAE %in% c(IMAE_CENEU, IMAE_NEPHROS, IMAE_DIAVERUM, IMAE_SARI) ~ "INDEPENDIENTE",
                ZCAIMAE %in% IMAE_PUBLICO ~ "PUBLICO",
                ZCAIMAE %in% IMAE_PRIVADO ~ "PRIVADO",
                TRUE ~ NA),
    privado=if_else(tipo_imae=="PRIVADO", 1, 0),
    indep=if_else(tipo_imae=="INDEPENDIENTE", 1, 0),
    publico=if_else(tipo_imae=="PUBLICO", 1, 0),
    tiene_imae=
      case_when(ZCASINST=="ASOCIACION ESPAÑOLA" ~ 1,
                ZCASINST=="SMI - SERVICIO MEDICO INTEGRAL" ~ 1,
                ZCASINST=="HOSPITAL BRITANICO" ~ 1,
                ZCASINST=="S.M.Q. SALTO" ~ 1,
                ZCASINST=="COMEPA" ~ 1,
                ZCASINST=="COMEF IAMPP" ~ 1,
                ZCASINST=="CASMU - IAMPP" ~ 1,
                ZCASINST=="CASA DE GALICIA" ~ 1,
                ZCASINST=="COSEM IAMPP" ~ 1,
                ZCASINST=="UNIVERSAL" ~ 1,
                ZCASINST=="HOSPITAL ITALIANO" ~ 1,
                ZCASINST=="ASSE" ~ 1,
                ZCASINST=="MUCAM" ~ 1,
                TRUE ~ 0),
    imae_inst=
      case_when(ZCAIMAE=="ASOCIACION ESPAÑOLA" & ZCASINST=="ASOCIACION ESPAÑOLA" ~ 1,
                ZCAIMAE=="SMI - SERVICIO MEDICO INTEGRAL" & ZCASINST=="SMI - SERVICIO MEDICO INTEGRAL" ~ 1,
                ZCAIMAE=="HOSPITAL BRITANICO" & ZCASINST=="HOSPITAL BRITANICO" ~ 1,
                ZCAIMAE=="S.M.Q. SALTO" & ZCASINST=="S.M.Q. SALTO" ~ 1,
                ZCAIMAE=="COMEPA" & ZCASINST=="COMEPA" ~ 1,
                ZCAIMAE=="COMEF" & ZCASINST=="COMEF IAMPP" ~ 1,
                ZCAIMAE=="CASMU" & ZCASINST=="CASMU - IAMPP" ~ 1,
                ZCAIMAE=="CASA DE GALICIA" & ZCASINST=="CASA DE GALICIA" ~ 1,
                ZCAIMAE=="NEPHROS" & ZCASINST=="COSEM IAMPP" ~ 1,
                
                ZCAIMAE=="UNIVERSAL" & ZCASINST=="UNIVERSAL" ~ 1,
                ZCAIMAE=="HOSPITAL ITALIANO" & ZCASINST=="UNIVERSAL" ~ 1,
                
                ZCAIMAE =="HOSPITAL DE CLINICAS" & ZCASINST=="ASSE" ~ 1,
                ZCAIMAE =="HOSPITAL MACIEL" & ZCASINST=="ASSE" ~ 1,
                ZCAIMAE =="CANMU" & ZCASINST=="MUCAM" ~ 1,
                TRUE ~ 0),
    transp=
      case_when(ZCAIMAE %in% c(IMAE_CENEU, IMAE_NEPHROS, IMAE_DIAVERUM) ~ 1,
                ZCAIMAE %in% IMAE_PUBLICO ~ 1,
                ZCAIMAE == "ASOCIACION ESPAÑOLA" ~ 1,
                ZCAIMAE == "SMI - SERVICIO MEDICO INTEGRAL" ~ 1,
                ZCAIMAE == "CASMU" ~ 1,
                ZCAIMAE == "CASA DE GALICIA" ~ 1,
                ZCAIMAE == "ASOCIACION ESPAÑOLA" ~ 1,
                ZCAIMAE == "HOSPITAL EVANGELICO" ~ 1,
                ZCAIMAE == "CANMU" ~ 0,
                ZCAIMAE == "HOSPITAL BRITANICO" ~ 0,
                ZCAIMAE == "UNIVERSAL" ~ 0,
                ZCAIMAE == "HOSPITAL ITALIANO" ~ 0,
                TRUE ~ NA),
    tipo_pac=
      case_when(ZB1SOCUP0 %in% PAC_ACTIVO ~ "Activo",
                ZB1SOCUP0 %in% PAC_INACTIVO ~ "Inactivo",
                TRUE ~ NA),
    
    fecha_solicitud=as.Date(CAFECSOL),
    fecha_autorizacion=as.Date(CAFECAUT),
    mes_solicitud=format(fecha_solicitud, "%Y-%m"),
    mes_autorizacion=format(fecha_autorizacion, "%Y-%m"),
    anio_solicitud=as.double(format(fecha_solicitud, "%Y")),
    anio_autorizacion=format(fecha_autorizacion, "%Y"),
    
    hab_op = case_when(
      ZCAIMAE == "ASOCIACION ESPAÑOLA" ~ 12,
      ZCAIMAE == "CANMU" ~ 18,
      ZCAIMAE == "CASA DE GALICIA" ~ 15,
      ZCAIMAE == "CASMU" & (anio_solicitud %in% c(2015, 2016)) ~ 36,
      ZCAIMAE == "CASMU" & (anio_solicitud %in% c(2013, 2014)) ~ 18,
      ZCAIMAE == "CASMU" & anio_solicitud<2013 ~ 15,
      ZCAIMAE == "CE.DI.SA." ~ 10,
      ZCAIMAE == "HOSPITAL BRITANICO" & (anio_solicitud %in% c(2014, 2015, 2016)) ~ 9,
      ZCAIMAE == "HOSPITAL BRITANICO" & anio_solicitud<2014 ~ 8,
      ZCAIMAE == "HOSPITAL DE CLINICAS" ~ 9,
      ZCAIMAE == "HOSPITAL EVANGELICO" ~ 11,
      ZCAIMAE == "HOSPITAL MACIEL" ~ 12,
      ZCAIMAE == "INTIR" ~ 12,
      ZCAIMAE == "INU" ~ 12,
      ZCAIMAE == "NEPHROS" ~ 12,
      ZCAIMAE == "RENIS" ~ 12,
      ZCAIMAE == "SEDIC" ~ 12,
      ZCAIMAE == "SMI - SERVICIO MEDICO INTEGRAL" ~ 12,
      ZCAIMAE == "UNIVERSAL" ~ 12,
      ZCAIMAE == "URUGUAYANA" ~ 15,
      ZCAIMAE == "SARI" ~ NA_real_,
      .default = NA_real_),
    
    turnos_op = case_when(
      ZCAIMAE %in% c("ASOCIACION ESPAÑOLA", "CANMU", "CASA DE GALICIA", "CASMU",
                     "HOSPITAL BRITANICO", "HOSPITAL EVANGELICO",
                     "INTIR", "RENIS", "SEDIC", "URUGUAYANA") ~ 6,
      ZCAIMAE %in% c("INU", "NEPHROS", "SMI - SERVICIO MEDICO INTEGRAL",
                     "UNIVERSAL") ~ 5,
      ZCAIMAE %in% c("CE.DI.SA.", "HOSPITAL DE CLINICAS") ~ 4,
      ZCAIMAE == "HOSPITAL MACIEL" ~ 6.333,
      ZCAIMAE == "SARI" ~ NA_real_,
      .default = NA_real_),
    
    edu_ningu=if_else(B1SNIVEL=="Ninguno", 1, 0),
    edu_prim=if_else(B1SNIVEL=="Primaria", 1, 0),
    edu_sec=if_else(B1SNIVEL=="Secundaria", 1, 0),
    edu_utu=if_else(B1SNIVEL=="UTU", 1, 0),
    edu_uni=if_else(B1SNIVEL=="Universidad", 1, 0),
    edu_prim_or_less=if_else(B1SNIVEL=="Ninguno" |
                               B1SNIVEL=="Primaria", 1, 0),
    edu_sec_or_more=if_else(B1SNIVEL=="Secundaria" |
                              B1SNIVEL=="UTU" |
                              B1SNIVEL=="Universidad", 1, 0),
    
    jubi=if_else(ZB1SOCUP0=="Jubilado", 1, 0)
    ) %>% 
  group_by(CAPACNUM) %>% 
  slice_max(CASEDADA, with_ties = FALSE)

write.csv(
  INGRESOS,
  "INGRESOS.csv", 
  row.names=FALSE)