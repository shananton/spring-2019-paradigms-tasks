-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name FROM Country
JOIN Capital ON Country.Code = Capital.CountryCode
JOIN City ON City.Id = Capital.CityId
WHERE Country.Name = "Malaysia";
