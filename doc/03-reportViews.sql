-- This file contains views for fairly complex to complex queries or queries that return interesting stuff (more than one report)

drop view if exists followupStatus;
DROP VIEW IF EXISTS forlopsoversikt;
DROP VIEW IF EXISTS skjemaoversikt;

create view followupStatus as 
select 
    FOLLOWUP_MCE, 
    CASE 
      WHEN p.DECEASED THEN MIN(m.MCE_COMPLETE)
      WHEN (COUNT(m.FOLLOWUP_MCE IS NOT NULL) = 2) and MIN(m.MCE_COMPLETE) = 1 then 2
      WHEN (COUNT(m.FOLLOWUP_MCE IS NOT NULL) < 2) and MAX(m.MCE_COMPLETE) = 1 then 1
      ELSE MIN(m.MCE_COMPLETE)
    END as COMPLETE_STATUS,              
    COUNT(m.FOLLOWUP_MCE IS NOT NULL AND m.Q1B_STATUS = 1) AS CNT_FOLLOWUPS
from mcelist m, patient p where m.SSN = p.SSN group by FOLLOWUP_MCE;    
  
create view forlopsoversikt AS  
  select
    CAST(m.MCEID AS CHAR) ForlopsID,
    m.CENTREID AS AvdRESH,
    (CASE MCETYPE
      WHEN 1 THEN q2b.OP_DATE
      WHEN 2 THEN IFNULL(q2as2.PROCDATE, q2as1.PROCDATE)
      WHEN 3 THEN q1b.FILLING_DATE
      WHEN 4 THEN q1b.FILLING_DATE
      ELSE NULL
    END) AS HovedDato,
    CAST((SELECT
    CASE MCETYPE
      WHEN 1 THEN IFNULL(LEAST(q1a.STATUS, q1b.STATUS, q2b.STATUS), -1)
      WHEN 2 THEN IFNULL(LEAST(q1a.STATUS, q1b.STATUS, q2as1.STATUS, IFNULL(q2as2.STATUS, 1)), -1)
      WHEN 3 THEN IFNULL(q1b.STATUS, -1)
      WHEN 4 THEN IFNULL(q1b.STATUS, -1)
      ELSE -1
    END 
    ) AS SIGNED) AS BasisRegStatus,               
    ifnull((fs.COMPLETE_STATUS), -2) as OppflgRegStatus,                 
    CAST(p.ID AS CHAR) AS PasientID,
    getFriendlyName(m.CENTREID) AS Sykehusnavn,
    CASE MCETYPE
      WHEN 1 THEN getAgeAtMainDate(m.MCEID)
      WHEN 2 THEN getAgeAtMainDate(m.MCEID)
      WHEN 3 THEN getAgeAtMainDate(m.FOLLOWUP_MCE)
      WHEN 4 THEN getAgeAtMainDate(m.FOLLOWUP_MCE)
      ELSE null
    END AS PasientAlder,
    CASE 
      WHEN IFNULL(p.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN p.GENDER = 1 THEN 'Mann' 
      WHEN p.GENDER = 2 THEN 'Kvinne'
      WHEN p.GENDER = 9 THEN 'Ikke relevant'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    p.SSN as KryptertFnr,
    CASE 
      WHEN p.GENDER = 1 THEN 1
      WHEN p.GENDER = 2 THEN 0
      ELSE NULL
    END AS ErMann,    
    getListText('MCE_MCETYPE', m.MCETYPE) AS ForlopsType1, 
    m.MCETYPE as ForlopsType1Num,    
    CASE m.SNMTYPE
      WHEN 1 THEN 'Test usikker'
      WHEN 2 THEN 'Test positiv' 
      WHEN 3 THEN 'Revisjon'
      WHEN 4 THEN 'Eksplantasjon'
      WHEN 5 THEN 'Test negativ'
      WHEN 0 THEN null
      ELSE null
    END AS ForlopsType2,       
    IF (m.SNMTYPE = 0, null, m.SNMTYPE)as ForlopsType2Num,
    m.FOLLOWUP_MCE as KobletForlopsID,
    CASE
      WHEN m.MCETYPE = 3 OR m.MCETYPE = 4 THEN TRUE
      ELSE FALSE
    END  AS ErOppfolging,
    p.ZIPCODE AS Postnr,       
    null as PostSted,
    null as Kommune,
    null as Fylke,
    CASE MCETYPE
      WHEN 1 THEN NULL
      WHEN 2 THEN NULL
      WHEN 3 THEN q1b.TIMING
      WHEN 4 THEN q1b.TIMING
      ELSE -1
    END AS OppflgSekNr, 
    CASE q1b.FOLLOWUP_POSSIBLE
        WHEN 0 THEN getListText('Q1B_FOLLOWUP_IMPOSSIBLE_REASON', q1b.FOLLOWUP_IMPOSSIBLE_REASON)
        WHEN 1 THEN 'Oppfølging mulig'
        ELSE NULL        
    END AS OppflgStatus       
 from
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
    LEFT OUTER JOIN followupStatus fs on fs.FOLLOWUP_MCE = m.MCEID
    LEFT OUTER JOIN centre c ON m.CENTREID = c.ID
    LEFT OUTER JOIN q1a q1a on m.MCEID = q1a.MCEID
    LEFT OUTER JOIN q1b q1b on m.MCEID = q1b.MCEID
    LEFT OUTER JOIN q2b q2b on m.MCEID = q2b.MCEID
    LEFT OUTER JOIN q2a q2as1 ON m.MCEID = q2as1.MCEID AND q2as1.STEP = 1
    LEFT OUTER JOIN q2a q2as2 ON q2as1.MCEID = q2as2.MCEID AND q2as2.STEP = 2;
    
    
create view skjemaoversikt AS
select 
  '1A Anamnese' AS Skjemanavn,
  skjema.STATUS AS SkjemaStatus,
  skjema.MCEID AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  1 as SkjemaRekkeflg
from
  q1a skjema,
  centre c
WHERE skjema.CENTREID = c.ID  
UNION
select 
  '1B Symptom' AS Skjemanavn,
  skjema.STATUS AS SkjemaStatus,
  skjema.MCEID AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH, 
  3 as SkjemaRekkeflg
from
  q1b skjema,
  centre c
WHERE skjema.CENTREID = c.ID  
UNION
select 
  '2A SNM-1' AS Skjemanavn,
  skjema.STATUS AS SkjemaStatus,
  skjema.MCEID AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  5 as SkjemaRekkeflg
from
  q2a skjema,
  centre c
WHERE skjema.CENTREID = c.ID and skjema.STEP = 2
UNION
select 
  '2A SNM-2' AS Skjemanavn,
  skjema.STATUS AS SkjemaStatus,
  skjema.MCEID AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  7 as SkjemaRekkeflg
from
  q2a skjema,
  centre c
WHERE skjema.CENTREID = c.ID  and skjema.STEP = 2
UNION
select 
  '2B Sfinkter' AS Skjemanavn,
  skjema.STATUS AS SkjemaStatus,
  skjema.MCEID AS ForlopsID, 
  skjema.CREATEDBY AS OpprettetAv,
  skjema.TSCREATED AS OpprettetDato,
  skjema.UPDATEDBY AS SistLagretAv,
  skjema.TSUPDATED AS SistLagretDato,
  getMainDate(skjema.MCEID) AS HovedDato,
  getFriendlyName(c.ID) AS Sykehusnavn,
  c.ID AS AvdRESH,
  9 as SkjemaRekkeflg
from
  q2b skjema,
  centre c
WHERE skjema.CENTREID = c.ID;  
        

drop view if exists allevarnum;
create view allevarnum as
SELECT m.MCEID AS ForlopsID,
       m.CENTREID AS AvdRESH,
       getFriendlyName(c.ID) AS Sykehusnavn,
       c.CENTRENAME AS senterNavn,
       c.CENTRESHORTNAME AS SenterKortNavn,       
        getListText('MCE_MCETYPE', m.MCETYPE) AS ForlopsType1, 
        m.MCETYPE as ForlopsType1Num,          
        CASE m.SNMTYPE
	      WHEN 1 THEN 'Test usikker'
	      WHEN 2 THEN 'Test positiv' 
	      WHEN 3 THEN 'Revisjon'
	      WHEN 4 THEN 'Eksplantasjon'
	      WHEN 5 THEN 'Test negativ'
	      WHEN 0 THEN null
	      ELSE null
        END AS ForlopsType2, 
        IF (m.SNMTYPE = 0, null, m.SNMTYPE)as ForlopsType2Num,
        m.FOLLOWUP_MCE as KobletForlopsID,     
        CASE
          WHEN m.MCETYPE = 3 OR m.MCETYPE = 4 THEN TRUE
          ELSE FALSE
        END  AS ErOppfolging,        
       m.SNMTYPE AS SNMType,
       p.ID AS PatientID,
       p.SSN AS KryptertFnr,
       p.LASTNAME AS EtterNavn,
       p.FIRSTNAME AS ForNavn,
       p.BIRTH_DATE AS FodselsDato,
       p.DECEASED AS Avdod,
       p.DECEASED_DATE AS DodsDato,
       p.GENDER AS PasientKjonn,
        CASE MCETYPE
          WHEN 1 THEN getAgeAtMainDate(m.MCEID)
          WHEN 2 THEN getAgeAtMainDate(m.MCEID)
          WHEN 3 THEN getAgeAtMainDate(m.FOLLOWUP_MCE)
          WHEN 4 THEN getAgeAtMainDate(m.FOLLOWUP_MCE)
          ELSE null
        END AS PasientAlder,        
       (CASE m.MCETYPE
           WHEN 1 THEN q2b.OP_DATE
           WHEN 2 THEN ifnull(q2as2.PROCDATE, q2as1.PROCDATE)           
           WHEN 3 THEN q1b.FILLING_DATE
           WHEN 4 THEN q1b.FILLING_DATE
           ELSE NULL
        END)
          AS HovedDato,
        IF (m.MCETYPE in (3, 4), 
          (select parent.parentRegDate FROM parentmce parent where m.FOLLOWUP_MCE = parent.parentMceId), 
          null) AS HovedForlopDato,          
        CAST((SELECT
        CASE MCETYPE
          WHEN 1 THEN IFNULL(LEAST(q1a.STATUS, q1b.STATUS, q2b.STATUS), -1)
          WHEN 2 THEN IFNULL(LEAST(q1a.STATUS, q1b.STATUS, q2as1.STATUS, IFNULL(q2as2.STATUS, 1)), -1)
          WHEN 3 THEN q1b.STATUS
          WHEN 4 THEN q1b.STATUS
          ELSE -1
        END 
        ) AS SIGNED) AS BasisRegStatus,  
		q1a.FILLING_DATE AS FyllDato1A,
    q1a.SYMPTOMDURATION AS Symtomvarighet,
    q1a.POSTMENOPAUSAL AS Postmenopausal,
    q1a.UNKNOWN AS Ukjent,
    q1a.OBSTERIC AS ObsteriskSkade,
    q1a.TRAUMA AS AnnetTraume,
    q1a.PERINEAL_ABSCESS AS PerinealAbscess,
    q1a.HEMORRHOIDS AS Hemoroidereksjon,
    q1a.RECTUM_RESECTION AS Rectumreseksjon,
    q1a.SPHINCTEROPLASTY AS Sfinkterotomi,
    q1a.OTHER_PELVICSURGERY AS AnnenBekkenKirurgi,
    q1a.NEUROLOGICAL_DISEASE AS NevrologiskSykdom,
    q1a.PERIPHERAL_NERVEDAMAGE AS PeriferNervskade,
    q1a.OTHER_ETHIOLOGY AS AnnetEtiologi,
    q1a.CONSERVATIVE_ACTIONS AS Konservativ,
    q1a.IRRIGATION AS Irrigasjon,
		q1a.N_TIBIALIS AS Tibialisstimulering,
    q1a.ANAL_INJECTION AS AnalInjection, 
		q1a.SACRALNERVESTIMLUATION AS SNM, 
		q1a.SPHINCTEROPLASTY AS Sfinkterplastikk, 
		q1a.RECTOPEXI AS Rectopexi, 
    q1a.PERINEAL_SURGERY AS KirurgiForRectumprolaps, 
		q1a.GRACILIS AS Gracilisplastikk, 
		q1a.OSTOMY AS Stomi, 
		q1a.FORMERTREATMENT_OTHER AS AnnetTidligereBeh, 
		q1a.CONSULTATION_AGE AS Alder,
		q1b.FILLING_DATE AS FyllDato1B,
    q1b.FOLLOWUP_POSSIBLE AS OppfoelgingMulig, 
		q1b.FOLLOWUP_RECEIVED_OTHER_TREATMENT AS AnnenBehandling, 
    q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_ARTIFICIAL_MUSCLE AS AnnenBehKunstigLukkemuskel, 
		q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_TIBIALIS_STIMULI AS AnnenBehTibalisStimulering, 
		q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_APPENDICOSTOMI AS AnnenBehAppendicostomi, 
		q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_SFINKTERPLASTICS AS AnnenBehSfinkterplastikk, 
		q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_SNM AS AnnenBehSNM, 
		q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_PELVISTRAINING AS AnnenBehBekkenbunnstrening, 
		q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_OTHER AS AnnenBehAnnen, 
		q1b.SOLID_STOOL_LEAKAGE AS FastAvfoering, 
		q1b.LIQUID_STOOL_LEAKAGE AS FlytendeAvfoering, 
		q1b.AIR_LEAKAGE AS Luft, 
		q1b.CHANGE_OF_LIFESTYLE AS Livsstilsendring, 
		q1b.PAD_NEED AS BindEllerPropp, 
		q1b.CONSTIPATING_MEDICINE AS ForstoppendeMedikament, 
		q1b.NO_POSTPONABILITY AS Urge, 
    q1b.STMARKS_TOTALSCORE AS StMarksTotalScore,
		q1b.URINE_LEAKAGE AS Urinlekkasje, 
    q1b.LIFE_QUALITY AS GenQol, 
		q1b.SEXUALITY AS QolSexualitet, 
		q1b.SATISFACTION AS Tilfredshet,
		q1b.ANAL_ULTRASOUND AS Ultralyd, 
		q1b.PARTIAL_DEFECT AS PartiellDefekt, 
		q1b.OUTER_DEFECT AS FullveggsdefektYtreSfinkter, 
    q1b.OUTER_DEFECT_EXTENT_FROM as YtreSfinkterFraKl,
    q1b.OUTER_DEFECT_EXTENT_TO as YtreSfinkterTilKl,
    q1b.OUTER_DEFECT_DURATION as YtreSfinkterAntTimer,
		q1b.INNER_DEFECT AS FullveggsdefektIndreSfinkter, 
    q1b.INNER_DEFECT_EXTENT_FROM as IndreSfinkterFraKl,
    q1b.INNER_DEFECT_EXTENT_TO as IndreSfinkterTilKl,
    q1b.OUTER_DEFECT_DURATION as IndreSfinkterAntTimer,
		q2as1.ID AS IdQ2A,		
		q2as1.TSUPDATED AS TsupdatedQ2A,
		q2as1.UPDATEDBY AS UpdatedByQ2A,
		q2as1.PROCDATE AS ProsedyreDato2A,
    q2as1.OPCODE_ABD60 as ABD60,
    q2as1.OPCODE_ABD65 as ABD65,
		q2as1.OPCODE_AEA24 as AEA24,
		q2as1.OPCODE_AEA20 as AEA20,
		q2as1.OPCODE_JHGX00 as JHGX00,
    q2as1.TESTPROCEDURE AS Testprosedyre, 
    q2as1.REVISIONPROCEDURE AS RevisjonsProsedyre, 
    q2as1.EXPLANTPROCEDURE AS EksplantasjonProsedyre, 
    q2as1.ANESTHESIA AS Anestesi2A, 
    q2as1.LOCALIZATION_LEFT_S2 as s2Venstre,
    q2as1.LOCALIZATION_RIGHT_S2 as s2Hoyre,
    q2as1.LOCALIZATION_LEFT_S3 as s3Venstre,
    q2as1.LOCALIZATION_RIGHT_S3 as s3Hoyre,
    q2as1.LOCALIZATION_LEFT_S4 as s4Venstre,
    q2as1.LOCALIZATION_RIGHT_S4 as s4Hoyre,
    q2as1.PEROP_ANTIBIOTICS AS AntibiotikaPerop, 
    q2as1.POSTOP_ANTIBIOTICS AS AntibiotikaPostop,      
	  q2as1.TEST_DURATION AS TestVarighet,
    q2as1.ENDDATE AS TestSluttDato,
		q2as1.INCONTINENCE_PRE AS InkontinensFoerTest,
		q2as1.INCONTINENCE_POST AS InkontinensUnderTest,
		q2as1.URGENCY_PRE AS UrgencyFoerTest,
		q2as1.URGENCY_POST AS UrgencyUnderTest,
		q2as1.DEFECATION_PRE AS AvfoeringerFoerTest,   
		q2as1.DEFECATION_POST AS AvfoeringerUnderTest,
		q2as1.LEAK_PRE AS LekkasjedagerFoer,
		q2as1.LEAK_POST AS LekkasjedagerUnder,
    q2as1.TESTCONCLUSION AS Testkonklusjon,  	
    q2as1.EXPLANT_REASON_EFFECT as UtenEffekt,
		q2as1.EXPLANT_REASON_INFECTION as Infeksjon,
    q2as1.EXPLANT_REASON_ELEC_PAIN as SmerterVElektrode,
		q2as1.EXPLANT_REASON_PAIN_EXT as SmerterUEX,
		q2as1.EXPLANT_REASON_STIM_PAIN as SmerterVStimulator,
		q2as1.EXPLANT_REASON_STIM_OTHER as AnnenAarsakEksplant,
    q2as1.COMPLICATION AS Komplikasjon,      
		q2as2.ID AS IdQ2AT2,
		q2as2.PROCDATE AS ProsedyreDato2AT2,
    q2as2.IMPLANTPROCEDURE AS ImplantasjonsProsedyre2AT2,
		q2as2.EXPLANTPROCEDURE AS EksplantasjonProsedyre2AT2,
    q2as2.OPCODE_ABD60 as ABD602AT2,
    q2as2.OPCODE_ABD65 as ABD652AT2,
    q2as2.OPCODE_AEA24 as AEA242AT2,
    q2as2.OPCODE_AEA20 as AEA202AT2,
    q2as2.OPCODE_JHGX00 as JHGX002AT2,
    q2as2.ANESTHESIA AS AnestesiT2, 
    q2as2.LOCALIZATION_LEFT_S2 as s2VenstreT2,
    q2as2.LOCALIZATION_RIGHT_S2 as s2HoyreT2,
    q2as2.LOCALIZATION_LEFT_S3 as s3VenstreT2,
    q2as2.LOCALIZATION_RIGHT_S3 as s3HoyreT2,
    q2as2.LOCALIZATION_LEFT_S4 as s4VenstreT2,
    q2as2.LOCALIZATION_RIGHT_S4 as s4HoyreT2, 
    q2as2.PEROP_ANTIBIOTICS AS AntibiotikaPeropT2, 
    q2as2.POSTOP_ANTIBIOTICS AS AntibiotikaPostopT2,   	
    q2as2.EXPLANT_REASON_EFFECT as UtenEffektT2,
		q2as2.EXPLANT_REASON_INFECTION as InfeksjonT2,
    q2as2.EXPLANT_REASON_ELEC_PAIN as SmerterVElektrodeT2,
		q2as2.EXPLANT_REASON_PAIN_EXT as SmerterUEXT2,
		q2as2.EXPLANT_REASON_STIM_PAIN as SmerterVStimulatorT2,
		q2as2.EXPLANT_REASON_STIM_OTHER as AnnenAarsakEksplantT2,
    q2as2.COMPLICATION AS KomplikasjonT2,      
		q2as2.TSUPDATED AS TsupdatedQ2AT2,
		q2as2.UPDATEDBY AS UpdatedByQ2AT2,
		q2b.OP_DATE AS OperasjonsDato2B,
    q2b.POSITION AS Leie,     
    q2b.BOWEL_EMPTYING AS Tommingsregime,
    q2b.ANESTHESIA AS Anestesi2B,
		q2b.ANTIBIOTICS_PRE AS AntibiotikaPerop2B,
    q2b.ANTIBIOTICS_POST AS AntibiotikaPostop2B,
    q2b.OVERLAPPING_SUTURE AS SuturEksterneSPH,
		q2b.SEPARATE_SUTURE AS SuturInterneSPH,
		q2b.LEVATOR_SUTURE AS Levatorsutur,
		q2b.PERINEAL_PLASTIC AS PerinealPlastikk,
		q2b.FISTULA_CLOSE AS LukkingFistel,
		q2b.FISTULA_CLOSE_TO AS VedFistellukking,
		q2b.PER_PERFORATION AS PeropPerfrasjon,
		q2b.COMPLICATIONS AS PostopKomplikasjoner,
    q2b.COMPLICATION_BLEEDING AS Bloedning,
    q2b.COMPLICATION_INFECTION AS Saarinfeksjon,
    q2b.COMPLICATION_DEHISENS AS Saardehisens,
		q2b.PHYSIOTHERAPY AS PostopFysioterapi,
		q1a.STATUS AS STATUS1a,
		q1b.STATUS AS STATUS1b,
		q2as2.STATUS AS STATUS2a,
		q2b.STATUS AS STATUS2b 
	from
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
    LEFT OUTER JOIN followupStatus fs on fs.FOLLOWUP_MCE = m.MCEID
    LEFT OUTER JOIN centre c ON m.CENTREID = c.ID
    LEFT OUTER JOIN q1a q1a on m.MCEID = q1a.MCEID
    LEFT OUTER JOIN q1b q1b on m.MCEID = q1b.MCEID
    LEFT OUTER JOIN q2b q2b on m.MCEID = q2b.MCEID
    LEFT OUTER JOIN q2a q2as1 ON m.MCEID = q2as1.MCEID AND q2as1.STEP = 1
    LEFT OUTER JOIN q2a q2as2 ON q2as1.MCEID = q2as2.MCEID AND q2as2.STEP = 2;      
    
    
    
drop view if exists alleVar;
create view alleVar as
SELECT m.MCEID AS ForlopsID,
    m.CENTREID AS AvdRESH,
    getFriendlyName(c.ID) AS Sykehusnavn,
    c.CENTRENAME AS senterNavn,
    c.CENTRESHORTNAME AS SenterKortNavn,       
    getListText('MCE_MCETYPE', m.MCETYPE) AS ForlopsType1, 
    m.MCETYPE as ForlopsType1Num,          
    CASE m.SNMTYPE
      WHEN 1 THEN 'Test usikker'
      WHEN 2 THEN 'Test positiv' 
      WHEN 3 THEN 'Revisjon'
      WHEN 4 THEN 'Eksplantasjon'
      WHEN 5 THEN 'Test negativ'
      WHEN 0 THEN null
      ELSE null
    END AS ForlopsType2, 
    IF (m.SNMTYPE = 0, null, m.SNMTYPE)as ForlopsType2Num,
    m.FOLLOWUP_MCE as KobletForlopsID,     
    CASE
          WHEN m.MCETYPE = 3 OR m.MCETYPE = 4 THEN TRUE
          ELSE FALSE
    END  AS ErOppfolging,        
    m.SNMTYPE AS SNMType,
    p.ID AS PatientID,
    p.SSN AS KryptertFnr,
    p.LASTNAME AS EtterNavn,
    p.FIRSTNAME AS ForNavn,
    p.BIRTH_DATE AS FodselsDato,
    getListText('PATIENT_DECEASED', p.DECEASED) AS Avdod, 
    p.DECEASED_DATE AS DodsDato,
    CASE 
          WHEN IFNULL(p.GENDER,0) = 0 THEN 'Ikke angitt'
          WHEN p.GENDER = 1 THEN 'Mann' 
          WHEN p.GENDER = 2 THEN 'Kvinne'
          WHEN p.GENDER = 9 THEN 'Ikke relevant'
          ELSE 'Ukjent'
    END AS PasientKjonn,
    CASE MCETYPE
          WHEN 1 THEN getAgeAtMainDate(m.MCEID)
          WHEN 2 THEN getAgeAtMainDate(m.MCEID)
          WHEN 3 THEN getAgeAtMainDate(m.FOLLOWUP_MCE)
          WHEN 4 THEN getAgeAtMainDate(m.FOLLOWUP_MCE)
          ELSE null
    END AS PasientAlder,        
    (CASE m.MCETYPE
           WHEN 1 THEN q2b.OP_DATE
           WHEN 2 THEN ifnull(q2as2.PROCDATE, q2as1.PROCDATE)           
           WHEN 3 THEN q1b.FILLING_DATE
           WHEN 4 THEN q1b.FILLING_DATE
           ELSE NULL
        END)
          AS HovedDato,
    IF (m.MCETYPE in (3, 4), 
          (select parent.parentRegDate FROM parentmce parent where m.FOLLOWUP_MCE = parent.parentMceId), 
          null) AS HovedForlopDato,          
        CAST((SELECT
        CASE MCETYPE
          WHEN 1 THEN IFNULL(LEAST(q1a.STATUS, q1b.STATUS, q2b.STATUS), -1)
          WHEN 2 THEN IFNULL(LEAST(q1a.STATUS, q1b.STATUS, q2as1.STATUS, IFNULL(q2as2.STATUS, 1)), -1)
          WHEN 3 THEN q1b.STATUS
          WHEN 4 THEN q1b.STATUS
          ELSE -1
        END 
    ) AS SIGNED) AS BasisRegStatus,  
	q1a.FILLING_DATE AS FyllDato1A,
    getListText('Q1A_SYMPTOMDURATION', q1a.SYMPTOMDURATION) AS Symtomvarighet,
    getListText('Q1A_POSTMENOPAUSAL', q1a.POSTMENOPAUSAL) AS Postmenopausal,
    getListText('Q1A_UNKNOWN', q1a.UNKNOWN) AS Ukjent,
    getListText('Q1A_OBSTERIC', q1a.OBSTERIC) AS ObsteriskSkade,
    getListText('Q1A_TRAUMA', q1a.TRAUMA) AS AnnetTraume,
    getListText('Q1A_PERINEAL_ABSCESS', q1a.PERINEAL_ABSCESS) AS PerinealAbscess,
    getListText('Q1A_HEMORRHOIDS', q1a.HEMORRHOIDS) AS Hemoroidereksjon,
    getListText('Q1A_RECTUM_RESECTION', q1a.RECTUM_RESECTION) AS Rectumreseksjon,
    getListText('Q1A_SPHINCTEROPLASTY', q1a.SPHINCTEROPLASTY) AS Sfinkterotomi,
    getListText('Q1A_OTHER_PELVICSURGERY', q1a.OTHER_PELVICSURGERY) AS AnnenBekkenKirurgi,
    getListText('Q1A_NEUROLOGICAL_DISEASE', q1a.NEUROLOGICAL_DISEASE) AS NevrologiskSykdom,
    getListText('Q1A_PERIPHERAL_NERVEDAMAGE', q1a.PERIPHERAL_NERVEDAMAGE) AS PeriferNervskade,
    getListText('Q1A_OTHER_ETHIOLOGY', q1a.OTHER_ETHIOLOGY) AS AnnetEtiologi,
    getListText('Q1A_CONSERVATIVE_ACTIONS', q1a.CONSERVATIVE_ACTIONS) AS Konservativ,
    getListText('Q1A_IRRIGATION', q1a.IRRIGATION) AS Irrigasjon,
	getListText('Q1A_N_TIBIALIS', q1a.N_TIBIALIS) AS Tibialisstimulering,
    getListText('Q1A_ANAL_INJECTION', q1a.ANAL_INJECTION) AS AnalInjection, 
	getListText('Q1A_SACRALNERVESTIMLUATION', q1a.SACRALNERVESTIMLUATION) AS SNM, 
	getListText('Q1A_SPHINCTEROPLASTY', q1a.SPHINCTEROPLASTY) AS Sfinkterplastikk, 
	getListText('Q1A_RECTOPEXI', q1a.RECTOPEXI) AS Rectopexi, 
    getListText('Q1A_PERINEAL_SURGERY', q1a.PERINEAL_SURGERY) AS KirurgiForRectumprolaps, 
	getListText('Q1A_GRACILIS', q1a.GRACILIS) AS Gracilisplastikk, 
	getListText('Q1A_OSTOMY', q1a.OSTOMY) AS Stomi, 
	getListText('Q1A_FORMERTREATMENT_OTHER', q1a.FORMERTREATMENT_OTHER) AS AnnetTidligereBeh, 
	q1a.CONSULTATION_AGE AS Alder,
	q1b.FILLING_DATE AS FyllDato1B,
    getListText('Q1B_FOLLOWUP_POSSIBLE', q1b.FOLLOWUP_POSSIBLE) AS OppfoelgingMulig, 
	getListText('Q1B_FOLLOWUP_RECEIVED_OTHER_TREATMENT', q1b.FOLLOWUP_RECEIVED_OTHER_TREATMENT) AS AnnenBehandling, 
    getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_ARTIFICIAL_MUSCLE', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_ARTIFICIAL_MUSCLE) AS AnnenBehKunstigLukkemuskel, 
	getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_TIBIALIS_STIMULI', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_TIBIALIS_STIMULI) AS AnnenBehTibalisStimulering, 
	getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_APPENDICOSTOMI', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_APPENDICOSTOMI) AS AnnenBehAppendicostomi, 
	getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_SFINKTERPLASTICS', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_SFINKTERPLASTICS) AS AnnenBehSfinkterplastikk, 
	getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_SNM', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_SNM) AS AnnenBehSNM, 
	getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_PELVISTRAINING', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_PELVISTRAINING) AS AnnenBehBekkenbunnstrening, 
	getListText('Q1B_FOLLOWUP_OTHER_TREATMENT_TYPE_OTHER', q1b.FOLLOWUP_OTHER_TREATMENT_TYPE_OTHER) AS AnnenBehAnnen, 
	getListText('Q1B_SOLID_STOOL_LEAKAGE', q1b.SOLID_STOOL_LEAKAGE) AS FastAvfoering, 
	getListText('Q1B_LIQUID_STOOL_LEAKAGE', q1b.LIQUID_STOOL_LEAKAGE) AS FlytendeAvfoering, 
	getListText('Q1B_AIR_LEAKAGE', q1b.AIR_LEAKAGE) AS Luft, 
	getListText('Q1B_CHANGE_OF_LIFESTYLE', q1b.CHANGE_OF_LIFESTYLE) AS Livsstilsendring, 
	getListText('Q1B_PAD_NEED', q1b.PAD_NEED) AS BindEllerPropp, 
	getListText('Q1B_CONSTIPATING_MEDICINE', q1b.CONSTIPATING_MEDICINE) AS ForstoppendeMedikament, 
	getListText('Q1B_NO_POSTPONABILITY', q1b.NO_POSTPONABILITY) AS Urge, 
    q1b.STMARKS_TOTALSCORE AS StMarksTotalScore,
	getListText('Q1B_URINE_LEAKAGE', q1b.URINE_LEAKAGE) AS Urinlekkasje, 
    getListText('Q1B_LIFE_QUALITY', q1b.LIFE_QUALITY) AS GenQol, 
	getListText('Q1B_SEXUALITY', q1b.SEXUALITY) AS QolSexualitet, 
	getListText('Q1B_SATISFACTION', q1b.SATISFACTION) AS Tilfredshet,
	getListText('Q1B_ANAL_ULTRASOUND', q1b.ANAL_ULTRASOUND) AS Ultralyd, 
	getListText('Q1B_PARTIAL_DEFECT', q1b.PARTIAL_DEFECT) AS PartiellDefekt, 
	getListText('Q1B_OUTER_DEFECT', q1b.OUTER_DEFECT) AS FullveggsdefektYtreSfinkter, 
    q1b.OUTER_DEFECT_EXTENT_FROM as YtreSfinkterFraKl,
    q1b.OUTER_DEFECT_EXTENT_TO as YtreSfinkterTilKl,
    q1b.OUTER_DEFECT_DURATION as YtreSfinkterAntTimer,
	getListText('Q1B_INNER_DEFECT', q1b.INNER_DEFECT) AS FullveggsdefektIndreSfinkter, 
    q1b.INNER_DEFECT_EXTENT_FROM as IndreSfinkterFraKl,
    q1b.INNER_DEFECT_EXTENT_TO as IndreSfinkterTilKl,
    q1b.OUTER_DEFECT_DURATION as IndreSfinkterAntTimer,
	q2as1.ID AS IdQ2A,		
	q2as1.TSUPDATED AS TsupdatedQ2A,
	q2as1.UPDATEDBY AS UpdatedByQ2A,
	q2as1.PROCDATE AS ProsedyreDato2A,
    getCheckText(q2as1.OPCODE_ABD60) as ABD60,
    getCheckText(q2as1.OPCODE_ABD65) as ABD65,
	getCheckText(q2as1.OPCODE_AEA24) as AEA24,
	getCheckText(q2as1.OPCODE_AEA20) as AEA20,
	getCheckText(q2as1.OPCODE_JHGX00) as JHGX00,
    getListText('Q2A_TESTPROCEDURE', q2as1.TESTPROCEDURE) AS Testprosedyre, 
    getListText('Q2A_REVISIONPROCEDURE', q2as1.REVISIONPROCEDURE) AS RevisjonsProsedyre, 
    getListText('Q2A_EXPLANTPROCEDURE', q2as1.EXPLANTPROCEDURE) AS EksplantasjonProsedyre, 
    getListText('Q2A_ANESTHESIA', q2as1.ANESTHESIA) AS Anestesi2A, 
    getCheckText(q2as1.LOCALIZATION_LEFT_S2) as s2Venstre,
    getCheckText(q2as1.LOCALIZATION_RIGHT_S2) as s2Hoyre,
    getCheckText(q2as1.LOCALIZATION_LEFT_S3) as s3Venstre,
    getCheckText(q2as1.LOCALIZATION_RIGHT_S3) as s3Hoyre,
    getCheckText(q2as1.LOCALIZATION_LEFT_S4) as s4Venstre,
    getCheckText(q2as1.LOCALIZATION_RIGHT_S4) as s4Hoyre,
    getListText('Q2A_PEROP_ANTIBIOTICS', q2as1.PEROP_ANTIBIOTICS) AS AntibiotikaPerop, 
    getListText('Q2A_POSTOP_ANTIBIOTICS', q2as1.POSTOP_ANTIBIOTICS) AS AntibiotikaPostop,      
  	q2as1.TEST_DURATION AS TestVarighet,
    q2as1.ENDDATE AS TestSluttDato,
	q2as1.INCONTINENCE_PRE AS InkontinensFoerTest,
	q2as1.INCONTINENCE_POST AS InkontinensUnderTest,
	q2as1.URGENCY_PRE AS UrgencyFoerTest,
	q2as1.URGENCY_POST AS UrgencyUnderTest,
	q2as1.DEFECATION_PRE AS AvfoeringerFoerTest,   
	q2as1.DEFECATION_POST AS AvfoeringerUnderTest,
	q2as1.LEAK_PRE AS LekkasjedagerFoer,
	q2as1.LEAK_POST AS LekkasjedagerUnder,
    getListText('Q2A_TESTCONCLUSION', q2as1.TESTCONCLUSION) AS Testkonklusjon,  	
    getCheckText(q2as1.EXPLANT_REASON_EFFECT) as UtenEffekt,
	getCheckText(q2as1.EXPLANT_REASON_INFECTION) as Infeksjon,
    getCheckText(q2as1.EXPLANT_REASON_ELEC_PAIN) as SmerterVElektrode,
	getCheckText(q2as1.EXPLANT_REASON_PAIN_EXT) as SmerterUEX,
	getCheckText(q2as1.EXPLANT_REASON_STIM_PAIN) as SmerterVStimulator,
	getCheckText(q2as1.EXPLANT_REASON_STIM_OTHER) as AnnenAarsakEksplant,
    getListText('Q2A_COMPLICATION', q2as1.COMPLICATION) AS Komplikasjon,      
	q2as2.ID AS IdQ2AT2,
	q2as2.PROCDATE AS ProsedyreDato2AT2,
    getListText('Q2A_IMPLANTPROCEDURE', q2as2.IMPLANTPROCEDURE) AS ImplantasjonsProsedyre2AT2,
	getListText('Q2A_EXPLANTPROCEDURE', q2as2.EXPLANTPROCEDURE) AS EksplantasjonProsedyre2AT2,
    getCheckText(q2as2.OPCODE_ABD60) as ABD602AT2,
    getCheckText(q2as2.OPCODE_ABD65) as ABD652AT2,
    getCheckText(q2as2.OPCODE_AEA24) as AEA242AT2,
    getCheckText(q2as2.OPCODE_AEA20) as AEA202AT2,
    getCheckText(q2as2.OPCODE_JHGX00) as JHGX002AT2,
    getListText('Q2A_ANESTHESIA', q2as2.ANESTHESIA) AS AnestesiT2, 
    getCheckText(q2as2.LOCALIZATION_LEFT_S2) as s2VenstreT2,
    getCheckText(q2as2.LOCALIZATION_RIGHT_S2) as s2HoyreT2,
    getCheckText(q2as2.LOCALIZATION_LEFT_S3) as s3VenstreT2,
    getCheckText(q2as2.LOCALIZATION_RIGHT_S3) as s3HoyreT2,
    getCheckText(q2as2.LOCALIZATION_LEFT_S4) as s4VenstreT2,
    getCheckText(q2as2.LOCALIZATION_RIGHT_S4) as s4HoyreT2, 
    getListText('Q2A_PEROP_ANTIBIOTICS', q2as2.PEROP_ANTIBIOTICS) AS AntibiotikaPeropT2, 
    getListText('Q2A_POSTOP_ANTIBIOTICS', q2as2.POSTOP_ANTIBIOTICS) AS AntibiotikaPostopT2,   	
    getCheckText(q2as2.EXPLANT_REASON_EFFECT) as UtenEffektT2,
	getCheckText(q2as2.EXPLANT_REASON_INFECTION) as InfeksjonT2,
    getCheckText(q2as2.EXPLANT_REASON_ELEC_PAIN) as SmerterVElektrodeT2,
	getCheckText(q2as2.EXPLANT_REASON_PAIN_EXT) as SmerterUEXT2,
	getCheckText(q2as2.EXPLANT_REASON_STIM_PAIN) as SmerterVStimulatorT2,
	getCheckText(q2as2.EXPLANT_REASON_STIM_OTHER) as AnnenAarsakEksplantT2,
    getListText('Q2A_COMPLICATION', q2as2.COMPLICATION) AS KomplikasjonT2,      
	q2as2.TSUPDATED AS TsupdatedQ2AT2,
	q2as2.UPDATEDBY AS UpdatedByQ2AT2,
	q2b.OP_DATE AS OperasjonsDato2B,
    getListText('Q2B_POSITION', q2b.POSITION) AS Leie,     
    getListText('Q2B_BOWEL_EMPTYING', q2b.BOWEL_EMPTYING) AS Tommingsregime,
    getListText('Q2B_ANESTHESIA', q2b.ANESTHESIA) AS Anestesi2B,
	getListText('Q2B_ANTIBIOTICS_PRE', q2b.ANTIBIOTICS_PRE) AS AntibiotikaPerop2B,
    getListText('Q2B_ANTIBIOTICS_POST', q2b.ANTIBIOTICS_POST) AS AntibiotikaPostop2B,
    getListText('Q2B_OVERLAPPING_SUTURE', q2b.OVERLAPPING_SUTURE) AS SuturEksterneSPH,
	getListText('Q2B_SEPARATE_SUTURE', q2b.SEPARATE_SUTURE) AS SuturInterneSPH,
	getListText('Q2B_LEVATOR_SUTURE', q2b.LEVATOR_SUTURE) AS Levatorsutur,
	getListText('Q2B_PERINEAL_PLASTIC', q2b.PERINEAL_PLASTIC) AS PerinealPlastikk,
	getListText('Q2B_FISTULA_CLOSE', q2b.FISTULA_CLOSE) AS LukkingFistel,
	getListText('Q2B_FISTULA_CLOSE_TO', q2b.FISTULA_CLOSE_TO) AS VedFistellukking,
	getListText('Q2B_PER_PERFORATION', q2b.PER_PERFORATION) AS PeropPerfrasjon,
	getListText('Q2B_COMPLICATIONS', q2b.COMPLICATIONS) AS PostopKomplikasjoner,
    getListText('Q2B_COMPLICATION_BLEEDING', q2b.COMPLICATION_BLEEDING) AS Bloedning,
    getListText('Q2B_COMPLICATION_INFECTION', q2b.COMPLICATION_INFECTION) AS Saarinfeksjon,
    getListText('Q2B_COMPLICATION_DEHISENS', q2b.COMPLICATION_DEHISENS) AS Saardehisens,
	getListText('Q2B_PHYSIOTHERAPY', q2b.PHYSIOTHERAPY) AS PostopFysioterapi,
	q1a.STATUS AS statusQ1A,
	q1b.STATUS AS statusQ1B,
    q2as1.STATUS AS statusQ2A_T1,    
	q2as2.STATUS AS statusQ2A_T2, 
	q2b.STATUS AS statusQ2B
	from
    mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
    LEFT OUTER JOIN followupStatus fs on fs.FOLLOWUP_MCE = m.MCEID
    LEFT OUTER JOIN centre c ON m.CENTREID = c.ID
    LEFT OUTER JOIN q1a q1a on m.MCEID = q1a.MCEID
    LEFT OUTER JOIN q1b q1b on m.MCEID = q1b.MCEID
    LEFT OUTER JOIN q2b q2b on m.MCEID = q2b.MCEID
    LEFT OUTER JOIN q2a q2as1 ON m.MCEID = q2as1.MCEID AND q2as1.STEP = 1
    LEFT OUTER JOIN q2a q2as2 ON q2as1.MCEID = q2as2.MCEID AND q2as2.STEP = 2;      
