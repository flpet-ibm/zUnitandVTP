           03 FSP-REQUEST-ID            PIC X(6).
           03 FSP-RETURN-CODE           PIC 9(2).
           03 FSP-CUSTOMER-NUM          PIC 9(10).
           03 FSP-REQUEST-SPECIFIC      PIC X(32482).
      *    Fields used in INQ All and ADD customer
           03 FSP-CUSTOMER-REQUEST REDEFINES FSP-REQUEST-SPECIFIC.
              05 FSP-FIRST-NAME         PIC X(10).
              05 FSP-LAST-NAME          PIC X(20).
              05 FSP-DOB                PIC X(10).
              05 FSP-HOUSE-NAME         PIC X(20).
              05 FSP-HOUSE-NUM          PIC X(4).
              05 FSP-POSTCODE           PIC X(8).
              05 FSP-NUM-POLICIES       PIC 9(3).
              05 FSP-PHONE-MOBILE       PIC X(20).
              05 FSP-PHONE-HOME         PIC X(20).
              05 FSP-EMAIL-ADDRESS      PIC X(100).
              05 FSP-POLICY-DATA        PIC X(32267).
