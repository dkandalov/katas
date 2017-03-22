SELECT o.product_id, sum(o.quantity), count(o.order_id)
FROM orders o LEFT JOIN product p ON o.product_id = p.product_id
WHERE o.dispatch_date >= '2015-08-01 22:40:50' AND p.available_from < '2016-07-01 22:40:50'
GROUP BY p.product_id
HAVING sum(o.quantity) < 10
