namespace OSTOR.ClinicalTrials.Reports

module Common =
    type Patient =
        { LastName: LastName
          FirstName: FirstName
          DateOfBirth: DateOfBirth
          MRN: MRN
          Gender: Gender }
    and LastName = LastName of string
    and FirstName = FirstName of string
    and DateOfBirth = DateOfBirth of System.DateTime
    and MRN = MRN of string
    and Gender = Male | Female
