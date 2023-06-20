                                        -- SQL Code for Chapter 3


                    -- 3.2.2 Data Cleaning

ALTER TABLE bank_statement..all_statements
DROP COLUMN basename, bank_layout, page, transaction_id, Date_cleared, Amount, Reported_page, Account_name, 
Statement_date, Period_from, Period_to, Cleared_balance, Opening_balance, Closing_balance, Currency, 
Statement_info, Bank_info, Account_info, Bank_info1, Account_info1, Balance;

                    -- 3.2.3 Data Transformation

ALTER TABLE bank_statement..all_statements
    ALTER COLUMN Time TIME;

ALTER TABLE bank_statement..all_statements
    ALTER COLUMN Date DATE;

ALTER TABLE bank_statement..all_statements
    ALTER COLUMN Description_1 VARCHAR(50);

ALTER TABLE bank_statement..all_statements
    ALTER COLUMN Description_2 VARCHAR(20);

ALTER TABLE bank_statement..all_statements
    ALTER COLUMN Receipts DECIMAl(10, 2);

ALTER TABLE bank_statement..all_statements
    ALTER COLUMN Payments DECIMAL(10, 2);

                                        -- Code for Chapter 5

                    -- Figure 5.1

-- Query 1
SELECT 
    Date, 
    CONCAT('£', CAST(SUM(Payments) AS VARCHAR)) AS Amount_Spent
FROM 
    bank_statement..all_statements
WHERE 
    Description_2 = 'DEB' OR Description_2 = 'DD'
GROUP BY 
    Date
HAVING 
    SUM(Payments) > 30
ORDER BY 
    Date;

                    -- Figure 5.2

-- Query 2
SELECT 
    YEAR(Date) AS Year, 
    MONTH(Date) AS Month, 
    CONCAT('£', CAST(SUM(Payments) AS VARCHAR)) AS TotalExpenses
FROM 
    bank_statement..all_statements
WHERE 
    Description_2 = 'DEB' OR Description_2 = 'DD'
GROUP BY 
    YEAR(Date), MONTH(Date)
ORDER BY 
    YEAR(Date), MONTH(Date);



                    -- Figure 5.3

-- Creating the Category column
ALTER TABLE bank_statement..all_statements
ADD Category VARCHAR(50);

-- Updating the Category column
UPDATE bank_statement..all_statements
SET Category = 
  CASE 
    WHEN Description_1 IN ('BURGER KING', 'UBER * EATS PEND', 'PAPA JOHNS SOUTHSE', 'GREGGS PLC', 'ULTIMATE TASTE', 'DOMINO S PIZZA', 'PORTLAND COFFEE SH', 'DOMINOS PIZZA') THEN 'Outside food'
    WHEN Description_1 IN ('STGCOACH/CTYLINK', 'trainline', 'FIRST HAMPSHIRE') THEN 'Transportation'
    WHEN Description_1 IN ('ICELAND', 'TESCO STORES 6841', 'MADINA HALAL MEAT', 'CO-OP GROUP FOOD', 'LIDL GB PORTSMOUTH', 'SAINSBURY''S S/MKT', 'POUNDLAND LTD 1210', 'TESCO STORES 3048', 'SOUTHERN CO-OP 000', 'TESCO STORE 3186', 'SAINSBURYS S/MKTS', 'SALAM FRATTON FOOD', 'SOUTHERN CO-OP 002', 'TESCO STORES 4611', 'Portsmouth Interna', 'MAMA LIT SPECIAL F') THEN 'GroceriesnUtility'
    WHEN Description_1 IN ('AMZNMktplace', 'Amazon.co.uk*165Q5', 'PAYPAL FUNDING', 'CDKEYS.COM', 'H & M', 'PAYPAL PAYMENT', 'Microsoft*Store', 'Lycamobile UK', 'Footasylum Limited', 'Udemy', 'BRITISH HEART FOUN', 'THE PERFUME SHOP') THEN 'Online purchases'
    WHEN Description_1 = 'Vue Entertainment' THEN 'Cinema'
	WHEN Description_1 = 'LIFE CHANG' THEN 'Haircut'
    ELSE 'Other'
  END;

-- Query 3
SELECT YEAR(Date) AS Year, MONTH(Date) AS Month, CONCAT('£', ROUND(AVG(Payments), 0)) AS AverageExpense
FROM bank_statement..all_statements
WHERE Category = 'GroceriesnUtility'
GROUP BY YEAR(Date), MONTH(Date);



