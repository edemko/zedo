WITH RECURSIVE
    deps(id) AS (
        VALUES (:root)
        UNION
        SELECT child_id FROM dependency JOIN deps ON (id = parent_id)
    )
SELECT * FROM target WHERE id IN deps AND id != :root;