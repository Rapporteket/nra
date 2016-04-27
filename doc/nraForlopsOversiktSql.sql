

create view ForlopsOversikt AS  
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
      WHEN 1 THEN 'Test og eventuell implantasjon'
      WHEN 2 THEN 'Test' 
      WHEN 3 THEN 'Revisjon'
      WHEN 4 THEN 'Eksplantasjon'
      WHEN 5 THEN 'Test eksplantasjon'
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


