-- Выведите название страны с максимальным уровнем грамотности по последним
-- данным, которые доступны для страны. В выводе: название страны и уровень
-- грамотности, именно в таком порядке и без лишних полей. (0,75 баллов)
SELECT Country.Name, MAX(Rate) FROM Country
JOIN (
    SELECT *, MIN(Year) FROM LiteracyRate
    GROUP BY CountryCode
) on Country.Code = CountryCode;
