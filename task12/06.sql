-- Выведите названия 20 городов с самым большим процентом от населения страны в
-- порядке убывания процента. В случае равенства процентов, отсортировать города
-- в порядке обратном лексикографическому (вывод: название города, его
-- население, население страны). (0,5 баллов)
SELECT City.Name, City.Population, Country.Population FROM City
JOIN Country ON City.CountryCode = Country.Code
ORDER BY (City.Population / Country.Population) DESC, City.Name DESC
LIMIT 20;
