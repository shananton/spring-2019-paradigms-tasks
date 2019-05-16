-- Для каждой страны выведите её имя и количество городов-миллионников.
-- Отсортировать вывод по убыванию числа городов-миллионников. Для стран с
-- равным числом городов - порядок лексикографический. Учтите, что в базе
-- данных могут быть страны без городов вообще (например, информации о городах
-- нет, или кто-то посчитал Антарктиду страной), для таких стран нужно
-- вывести 0 (0,75 баллов).
SELECT Country.Name, COUNT(CountryCode) AS CityCount FROM Country
LEFT JOIN (
    SELECT City.CountryCode FROM City WHERE City.Population >= 1000000
) ON Country.Code = CountryCode
GROUP BY Country.Code
ORDER BY CityCount DESC, Country.Name ASC;
