-- Выведите форму правления с максимальной суммарной площадью стран, которые её
-- придерживаются (вывод: форма правления и суммарная площадь). (0,25 баллов)
SELECT Country.GovernmentForm, SUM(Country.SurfaceArea) AS TotalArea FROM Country
GROUP BY Country.GovernmentForm
ORDER BY TotalArea DESC
LIMIT 1;
